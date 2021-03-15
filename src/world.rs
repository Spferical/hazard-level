use enum_map::{enum_map, Enum, EnumMap};
use indexmap::map::IndexMap;
use lazy_static::lazy_static;
use rand::seq::SliceRandom;
use rand::Rng;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::f64::consts::PI;
use std::ops::Div;
use std::ops::Sub;
use std::ops::{Add, AddAssign, Index, IndexMut, Mul};

use crate::fov;
use crate::map_gen;

pub const CHUNKSIZE: usize = 16;
pub const FOV_RANGE: i32 = 8;
pub const PLAYER_MAX_HEALTH: i32 = 10;
pub const MOB_DESCRIPTION_LEN: usize = 30;

pub const EMERGENCY_ANNOUNCEMENTS: [&'static str; 10] =
    ["IGOR thanks you for helping keep our facility clean.",
     "High core temperature detected.",
     "Plant security is everyone's responsibility. If you \
      see something, say something!",
     "Hazard containment level 4. Please shelter in place.",
     "Researchers on the IGOR project are contributing \
      to a project that furthers all of humanity.",
     "Did you know that *0* people are working in this \
      facility right now?",
     "Most accidents are preventable.",
     "IGOR is there for you whenever you need him.",
     "Need to take a day off? Go ahead! Researchers at IGOR \
      have unlimited PTO.",
     "IGOR cares about you."
    ];

macro_rules! round_down {
    ($n:expr, $d:expr) => {
        if $n >= 0 {
            ($n / $d) * $d
        } else {
            (($n - $d + 1) / $d) * $d
        }
    };
}

macro_rules! modulo {
    ($n:expr, $d:expr) => {
        (($n % $d) + $d) % $d
    };
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Pos {
    pub x: i32,
    pub y: i32,
}

impl Pos {
    pub fn new(x: i32, y: i32) -> Pos {
        Pos { x, y }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Offset {
    pub x: i32,
    pub y: i32,
}

impl Offset {
    pub fn diag_dist(self) -> i32 {
        self.x.abs().max(self.y.abs())
    }
    pub fn mhn_dist(self) -> i32 {
        self.x.abs() + self.y.abs()
    }

    pub fn closest_dir(self) -> Self {
        let angle = (self.y as f64).atan2(self.x as f64);
        let mut octant = (8f64 * angle / (2f64 * PI) + 8f64) as usize % 8;
        if octant % 2 == 1 {
            octant -= 1;
        }
        DIRECTIONS[octant]
    }

    pub fn norm(self) -> Self {
        Offset {
            x: self.x.signum(),
            y: self.y.signum(),
        }
    }

    pub fn rot_cw(self) -> Self {
        Offset {
            x: self.y,
            y: -self.x,
        }
    }

    pub fn flip(self) -> Self {
        self.rot_cw().rot_cw()
    }

    pub fn rot_ccw(self) -> Self {
        self.flip().rot_cw()
    }
}

impl Mul<i32> for Offset {
    type Output = Offset;

    fn mul(self, x: i32) -> Offset {
        Offset {
            x: self.x * x,
            y: self.y * x,
        }
    }
}

impl Div<i32> for Offset {
    type Output = Offset;
    fn div(self, x: i32) -> Offset {
        Offset {
            x: self.x / x,
            y: self.y / x,
        }
    }
}

// Ordered by increasing angles, starting in the positive x direction.
pub const DIRECTIONS: [Offset; 8] = [
    Offset { x: 1, y: 0 },
    Offset { x: 1, y: 1 },
    Offset { x: 0, y: 1 },
    Offset { x: -1, y: 1 },
    Offset { x: -1, y: 0 },
    Offset { x: -1, y: -1 },
    Offset { x: 0, y: -1 },
    Offset { x: 1, y: -1 },
];

pub const CARDINALS: [Offset; 4] = [
    Offset { x: 0, y: 1 },
    Offset { x: 0, y: -1 },
    Offset { x: 1, y: 0 },
    Offset { x: -1, y: 0 },
];

impl Add<Offset> for Pos {
    type Output = Pos;

    fn add(self, offset: Offset) -> Pos {
        Pos {
            x: self.x + offset.x,
            y: self.y + offset.y,
        }
    }
}

impl Sub<Pos> for Pos {
    type Output = Offset;

    fn sub(self, other: Pos) -> Offset {
        Offset {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl AddAssign<Offset> for Pos {
    fn add_assign(&mut self, o: Offset) {
        self.x += o.x;
        self.y += o.y;
    }
}

fn get_chunk_index(pos: Pos) -> ChunkIndex {
    ChunkIndex {
        x: round_down!(pos.x, CHUNKSIZE as i32) / CHUNKSIZE as i32,
        y: round_down!(pos.y, CHUNKSIZE as i32) / CHUNKSIZE as i32,
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct ChunkIndex {
    pub x: i32,
    pub y: i32,
}

impl ChunkIndex {
    #[allow(dead_code)]
    fn topleft(&self) -> Pos {
        Pos {
            x: self.x * CHUNKSIZE as i32,
            y: self.y * CHUNKSIZE as i32,
        }
    }
}

#[derive(Enum, PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum TileKind {
    Floor,
    Wall,
    Ocean,
    BlackFloor,
    YellowFloor,
    YellowWall,
    BloodyFloor,
    Computer,
    Fire,
    // represents unseen tile in player memory World -- should never actually exist
    Unseen,
}

#[derive(Enum, PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum MobKind {
    Zombie,
    Alien,
    OldMan,
    Sculpture,
}

fn redact(txt: &str, rng: &mut impl Rng) -> String {
    let sample_len = MOB_DESCRIPTION_LEN.min(txt.len());
    let start_index = rng.gen_range(0..(txt.len() - sample_len));

    let txt_substr = &txt[start_index..(start_index + sample_len)];
    let redacted = txt_substr
        .chars()
        .map(|a| if rng.gen::<f32>() < 0.1 { '?' } else { a })
        .collect::<String>();
    return format!("...{}...", redacted);
}

impl MobKind {
    fn max_health(&self) -> u32 {
        match self {
            Self::Zombie => 2,
            Self::OldMan => 10,
            Self::Alien => 1,
            Self::Sculpture => 99,
        }
    }

    pub fn mob_description(&self, rng: &mut impl Rng) -> String {
        redact(
            match self {
                Self::Zombie => "As the ??? infection progresses, \
                                 the patient's retina disintegrates, \
                                 rendering them partially blind. Patients \
                                 usually rely on sound and smell to locate \
                                 food and water. Given medical scans have been \
                                 unable to determine a neurological cause for \
                                 the observed behaviors, we suspect that the \
                                 aggression is at least partially psychological. There \
                                 is no know cure for ???; palliative care is \
                                 recommended. Symptoms typically begin with \
                                 memory loss, followed by difficulty \
                                 forming coherent sentences, disorientation, \
                                 paranoia, hallucinations, loss of vision, and eventually \
                                 aggressive and violent behavior. Paradoxically, \
                                 the infected often claim to observe \"shadowy\" \
                                 figures hiding in the periphery of their deteriorating \
                                 vision.",
                Self::OldMan => "As of [REDACTED], our researchers have not been \
                                 able to determine the origin of ????-???-?????. \
                                 One suspicion is that [REDACTED], also known as \"RED_MAN\", \
                                 originally emerged from [REDACTED] during the [REDACTED] \
                                 incident of 2083. In any case, it is essential that RED_MAN \
                                 is contained within its toroidal confinement zone. RED_MAN \
                                 has the unique capacity to pass through solid matter, and does \
                                 so in an effort to hunt down living creatures. RED_MAN has an \
                                 unusually adapatable, omnivorous diet, and can digest most forms of \
                                 plant and animal matter. In one particularly extreme example, RED_MAN \
                                 completely consumed a [REDACTED] carcass over the course of 1.5 hours. \
                                 The institue contains RED_MAN by exploiting its sensitivity to \
                                 strong magnetic fields. The containment chamber uses a tokamak-like \
                                 setup to prevent RED_MAN from escaping and consuming facility \
                                 personnel.",
                Self::Alien => "Our partners at [REDACTED] managed to procure a sample of \
                                [REDACTED] on ??/??/2124. While the supplier would not \
                                respond to inquires regarding the origin of [REDACTED], \
                                a more thorough analysis of [REDACTED] believe it must be \
                                related to the specimen recovered from LV-426. The cell \
                                wall of skin tissues from [REDACTED], which we nicknamed \
                                GRAY_TICK, has a thick layer of protein polysacchrides, \
                                which enhance its resistance to adverse environmental \
                                conditions. Unlike the LV-426 specimen, GRAY_TICK is \
                                unable to reproduce without externally-introduced \
                                microsperm. The lifecycle of GRAY_TICK resembles \
                                primitive Earth-basd bryophytes, and we suspect that \
                                the microsperm limitation may have introduced as a form of \
                                genetic engineering; a biological failsafe to prevent \
                                the explosive growth of [REDACTED].",
                Self::Sculpture => "At the moment, facility personnel are advised to \
                                    maintain a 15-foot distance from [REDACTED], also \
                                    known as BLUE_MONK. While BLUE_MONK may not \
                                    appear threatening, we have only started to \
                                    understand the effects of BLUE_MONK on the \
                                    human mind. BLUE_MONK is not capable of moving \
                                    or producing sound when it is being observed. \
                                    However, when it is _not_ being directly observed \
                                    facility personnel have noticed high-pitched \
                                    vocalizations from BLUE_MONK's containment \
                                    facility. When not directly within line-of-sight \
                                    of facility personnel, BLUE_MONK is capable of \
                                    moving at a speed of 15 meters per second, and
                                    is strong enough to shatter reinforced glass.
                                    Despite its friendly appearance, and comparisons \
                                    drawn between the vocalizations and human singing, \
                                    we urge extreme caution when around BLUE_MONK. Do not \
                                    let it leave your line of sight.",
            },
            rng,
        )
    }
}

pub struct TileKindInfo {
    pub opaque: bool,
    pub walkable: bool,
}

lazy_static! {
    pub static ref TILE_INFOS: EnumMap<TileKind, TileKindInfo> = enum_map! {
        TileKind::Floor => TileKindInfo {
            opaque: false,
            walkable: true,
        },
        TileKind::Wall => TileKindInfo {
            opaque: true,
            walkable: false,
        },
        TileKind::YellowWall => TileKindInfo {
            opaque: true,
            walkable: false,
        },
        TileKind::Unseen => TileKindInfo {
            opaque: true,
            walkable: false,
        },
        TileKind::Ocean => TileKindInfo {
            opaque: false,
            walkable: false,
        },
        TileKind::BlackFloor => TileKindInfo {
            opaque: false,
            walkable: true,
        },
        TileKind::YellowFloor => TileKindInfo {
            opaque: false,
            walkable: true,
        },
        TileKind::BloodyFloor => TileKindInfo {
            opaque: false,
            walkable: true,
        },
        TileKind::Computer => TileKindInfo {
            opaque: true,
            walkable: true,
        },
        TileKind::Fire => TileKindInfo {
            opaque: false,
            walkable: false,
        },
    };
}

impl TileKind {
    pub fn is_opaque(self) -> bool {
        TILE_INFOS[self].opaque
    }

    pub fn is_walkable(self) -> bool {
        TILE_INFOS[self].walkable
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum Item {
    Corpse,
    Ammo,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Tile {
    pub kind: TileKind,
    pub blood: bool,
    pub rust: bool,
    pub item: Option<Item>,
}

impl Tile {
    const fn from_kind(kind: TileKind) -> Self {
        Self {
            kind,
            blood: false,
            rust: false,
            item: None,
        }
    }
}

#[derive(Hash, Debug, Clone)]
pub struct Mob {
    pub kind: MobKind,
    pub damage: u32,
    pub saw_player_at: Option<Pos>,
    pub patrol: Option<Vec<Pos>>,
    pub patrol_idx: Option<usize>,
    pub alien_state: i32,
}

impl Mob {
    pub fn new(kind: MobKind) -> Self {
        Self {
            kind,
            damage: 0,
            saw_player_at: None,
            patrol: None,
            patrol_idx: None,
            alien_state: 0,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct Chunk {
    grid: [[Tile; CHUNKSIZE]; CHUNKSIZE],
}

const WALL_CHUNK: Chunk = Chunk {
    grid: [[Tile::from_kind(TileKind::Wall); CHUNKSIZE]; CHUNKSIZE],
};

const UNSEEN_CHUNK: Chunk = Chunk {
    grid: [[Tile::from_kind(TileKind::Unseen); CHUNKSIZE]; CHUNKSIZE],
};

#[derive(Debug, Clone)]
pub struct Thing {
    pub pos: Pos,
    pub elapsed: f32,
}

#[derive(Debug, Clone)]
pub struct World {
    chunks: IndexMap<ChunkIndex, Chunk>,
    default_chunk: &'static Chunk,
    pub mobs: HashMap<Pos, Mob>,
    player_pos: Pos,
    player_damage: i32,
    pub player_ammo: u32,
    pub thing: Thing,
    pub pending_announcements: Vec<(Vec<Rect>, &'static str)>,
}

impl Index<Pos> for World {
    type Output = Tile;

    fn index(&self, pos: Pos) -> &Tile {
        let chunk_index = get_chunk_index(pos);
        let chunk = self.chunks.get(&chunk_index).unwrap_or(self.default_chunk);
        let chunk_offset_x = modulo!(pos.x, CHUNKSIZE as i32);
        let chunk_offset_y = modulo!(pos.y, CHUNKSIZE as i32);
        &chunk.grid[chunk_offset_x as usize][chunk_offset_y as usize]
    }
}

impl IndexMut<Pos> for World {
    fn index_mut(&mut self, pos: Pos) -> &mut Tile {
        let chunk_index = get_chunk_index(pos);
        let chunk = self
            .chunks
            .entry(chunk_index)
            .or_insert(*self.default_chunk);
        let chunk_offset_x = modulo!(pos.x, CHUNKSIZE as i32);
        let chunk_offset_y = modulo!(pos.y, CHUNKSIZE as i32);
        &mut chunk.grid[chunk_offset_x as usize][chunk_offset_y as usize]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Rect {
    pub x1: i32,
    pub y1: i32,
    pub x2: i32,
    pub y2: i32,
}

impl Rect {
    pub fn topleft(&self) -> Pos {
        Pos {
            x: self.x1,
            y: self.y1,
        }
    }
    pub fn topright(&self) -> Pos {
        Pos {
            x: self.x2,
            y: self.y1,
        }
    }
    pub fn bottomleft(&self) -> Pos {
        Pos {
            x: self.x1,
            y: self.y2,
        }
    }
    pub fn bottomright(&self) -> Pos {
        Pos {
            x: self.x2,
            y: self.y2,
        }
    }
    pub fn width(&self) -> i32 {
        self.x2 - self.x1
    }
    pub fn height(&self) -> i32 {
        self.y2 - self.y1
    }
    pub fn choose(&self, rng: &mut impl Rng) -> Pos {
        let x = rng.gen_range(self.x1..=self.x2);
        let y = rng.gen_range(self.y1..=self.y2);
        Pos { x, y }
    }
    pub fn new(x1: i32, x2: i32, y1: i32, y2: i32) -> Self {
        Rect { x1, y1, x2, y2 }
    }
    pub fn smol(pos: Pos) -> Self {
        Self::new(pos.x, pos.x, pos.y, pos.y)
    }
    pub fn expand(mut self, amt: i32) -> Self {
        self.x1 -= amt;
        self.x2 += amt;
        self.y1 -= amt;
        self.y2 += amt;
        self
    }

    pub fn contains(&self, pos: Pos) -> bool {
        pos.x >= self.x1 && pos.x <= self.x2 && pos.y >= self.y1 && pos.y <= self.y2
    }

    pub fn center(&self) -> Pos {
        Pos {
            x: avg!(self.x1, self.x2),
            y: avg!(self.y1, self.y2),
        }
    }
}

pub struct RectIter {
    rect: Rect,
    idx: i32,
}

impl Iterator for RectIter {
    type Item = Pos;
    fn next(&mut self) -> std::option::Option<Pos> {
        let width = self.rect.x2 - self.rect.x1 + 1;
        let height = self.rect.y2 - self.rect.y1 + 1;
        if self.idx >= width * height {
            None
        } else {
            let x = self.rect.x1 + (self.idx % width);
            let y = self.rect.y1 + (self.idx / width);
            self.idx += 1;
            Some(Pos { x, y })
        }
    }
}

impl IntoIterator for Rect {
    type Item = Pos;
    type IntoIter = RectIter;
    fn into_iter(self) -> Self::IntoIter {
        RectIter { rect: self, idx: 0 }
    }
}

pub enum Effect {
    Shuffle,
    Creak,
    Hiss,
    Scrape,
    Sing,
}

impl World {
    fn new(default_chunk: &'static Chunk) -> Self {
        World {
            chunks: IndexMap::new(),
            default_chunk,
            mobs: HashMap::new(),
            player_pos: Pos { x: 0, y: 0 },
            player_damage: 0,
            player_ammo: 32,
            thing: Thing {
                pos: Pos { x: -3, y: 0 },
                elapsed: 0.0,
            },
            pending_announcements: Vec::new(),
        }
    }

    pub fn move_player(&mut self, offset: Offset, force: bool) -> bool {
        let new_pos = self.player_pos + offset;
        if self[new_pos].kind.is_walkable() && self.mobs.get(&new_pos).is_none() || force {
            self.player_pos += offset;
            true
        } else {
            false
        }
    }

    fn damage_mob(&mut self, pos: Pos) {
        self[pos].blood = true;
        let mob = self.mobs.get_mut(&pos).unwrap();
        mob.damage += 1;
        if mob.damage >= mob.kind.max_health() {
            self.mobs.remove(&pos);
            if self[pos].item.is_none() {
                self[pos].item = Some(Item::Corpse);
            }
        }
    }

    fn damage_player(&mut self) {
        self.player_damage += 1;
    }

    pub fn player_damage(&self) -> i32 {
        self.player_damage
    }

    pub fn fire(&mut self, start: Pos, off: Offset) -> Option<Pos> {
        if self.player_ammo == 0 {
            return None;
        }
        self.player_ammo -= 1;
        let mut pos = start;
        Some(loop {
            pos += off;
            if !TILE_INFOS[self[pos].kind].walkable && self[pos].kind != TileKind::Ocean {
                break pos;
            } else if self.mobs.get_mut(&pos).is_some() {
                self.damage_mob(pos);
                break pos;
            }
        })
    }

    fn move_towards(
        &mut self,
        pos: Pos,
        target: Pos,
        through_walls: bool,
        around_mobs: bool,
        range: Option<usize>,
    ) -> Pos {
        let range = range.unwrap_or(FOV_RANGE as usize * 3);
        let off = self.path(pos, target, range, through_walls, around_mobs);
        if let Some(off) = off {
            let new_pos = pos + off;
            if !self.mobs.contains_key(&new_pos) {
                new_pos
            } else {
                pos
            }
        } else {
            pos
        }
    }

    fn pursue_if_seen_player(
        &mut self,
        mob: &mut Mob,
        pos: Pos,
        through_walls: bool,
        around_mobs: bool,
    ) -> Option<Pos> {
        let already_pursuing_player = mob.saw_player_at.is_some();
        if already_pursuing_player {
            if let Some(target) = mob.saw_player_at {
                if target == self.player_pos && (target - pos).mhn_dist() <= 1 {
                    self.player_damage += 1;
                    Some(pos)
                } else {
                    let pos = self.move_towards(pos, target, through_walls, around_mobs, None);
                    if pos == target {
                        mob.saw_player_at = None;
                    }
                    Some(pos)
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    fn patrol(&mut self, mob: &mut Mob, pos: Pos) -> Pos {
        if let Some(ref patrol) = mob.patrol {
            let idx = mob.patrol_idx.get_or_insert(0);
            let mut goal = patrol[*idx];
            if pos == goal {
                *idx += 1;
                *idx %= patrol.len();
                goal = patrol[*idx];
            }
            self.move_towards(pos, goal, true, true, None)
        } else {
            pos
        }
    }

    fn update_thing(&mut self, dt: f32, _effects: &mut Vec<(Pos, Effect)>, _rng: &mut impl Rng) {
        let old_elapsed = self.thing.elapsed;
        self.thing.elapsed += dt;
        let thing_dist = |x: f32| (x * (x / 300.0)) as i32;
        let player_dist = (self.thing.pos - self.player_pos).mhn_dist() as usize * 4;
        for _ in thing_dist(old_elapsed)..thing_dist(self.thing.elapsed) {
            if (self.thing.pos - self.player_pos).mhn_dist() <= 1 {
                self.damage_player()
            } else {
                let new_pos = self.move_towards(
                    self.thing.pos,
                    self.player_pos,
                    false,
                    true,
                    Some(player_dist),
                );
                self.thing.pos = new_pos;
            }
        }
    }

    fn update_mobs(&mut self, effects: &mut Vec<(Pos, Effect)>, rng: &mut impl Rng) {
        let seen = fov::calculate_fov(self.player_pos, FOV_RANGE, &self);
        let poses = self.mobs.keys().copied().collect::<Vec<_>>();
        for pos in poses {
            let mut mob = self.mobs.remove(&pos).unwrap();
            let new_pos = match mob.kind {
                MobKind::Sculpture => {
                    let mut new_pos = pos;
                    if seen.contains(&pos) {
                        mob.saw_player_at = Some(self.player_pos);
                    } else {
                        if rng.gen::<f32>() < 0.05 {
                            effects.push((new_pos, Effect::Sing));
                        }
                        for _ in 0..5 {
                            let next_pos = self
                                .pursue_if_seen_player(&mut mob, new_pos, false, true)
                                .unwrap_or_else(|| self.patrol(&mut mob, new_pos));
                            if seen.contains(&next_pos) || next_pos == new_pos {
                                break;
                            }
                            new_pos = next_pos;
                            if rng.gen::<f32>() < 0.01 {
                                effects.push((new_pos, Effect::Scrape));
                            }
                        }
                    }
                    new_pos
                }
                MobKind::OldMan => {
                    let range = 5;
                    let influence = Rect::smol(pos).expand(range);
                    for pos in influence.into_iter() {
                        self[pos].rust = false;
                    }
                    if influence.contains(self.player_pos) {
                        if mob.saw_player_at.is_none() {
                            effects.push((pos, Effect::Creak));
                        }
                        mob.saw_player_at = Some(self.player_pos);
                    }
                    if rng.gen::<f32>() < 0.0001 {
                        effects.push((pos, Effect::Creak));
                    }
                    let new_pos = self
                        .pursue_if_seen_player(&mut mob, pos, true, true)
                        .unwrap_or_else(|| self.patrol(&mut mob, pos));
                    let influence = Rect::smol(new_pos).expand(range);
                    for new_pos in influence.into_iter() {
                        self[new_pos].rust = true;
                    }
                    new_pos
                }
                MobKind::Zombie => {
                    if seen.contains(&pos) {
                        mob.saw_player_at = Some(self.player_pos);
                    }

                    if rng.gen::<f32>() < 0.0001 {
                        effects.push((pos, Effect::Shuffle));
                    }

                    self.pursue_if_seen_player(&mut mob, pos, false, false)
                        .unwrap_or(pos)
                }
                MobKind::Alien => {
                    if seen.contains(&pos) {
                        mob.saw_player_at = Some(self.player_pos);
                    }
                    let mut pos = pos;
                    if mob.saw_player_at.is_some() {
                        if mob.alien_state == 2 {
                            effects.push((pos, Effect::Hiss));
                        } else if mob.alien_state == 3 {
                            for _ in 0..4 {
                                pos = self
                                    .pursue_if_seen_player(&mut mob, pos, false, true)
                                    .unwrap_or(pos);
                            }
                        } else {
                            pos = self
                                .pursue_if_seen_player(&mut mob, pos, false, false)
                                .unwrap_or(pos);
                        }
                        mob.alien_state += 1;
                        mob.alien_state %= 4;
                    }
                    pos
                }
            };
            self.mobs.insert(new_pos, mob);
        }
    }

    pub fn tick(&mut self, dt: f32, player_moved: bool, rng: &mut impl Rng) -> Vec<(Pos, Effect)> {
        let mut effects = Vec::new();
        if player_moved {
            self.update_mobs(&mut effects, rng);
        }
        self.update_thing(dt, &mut effects, rng);
        effects
    }

    pub fn get_announcement(&mut self) -> Option<String> {
        self.pending_announcements
            .iter()
            .position(|rm| rm.0.iter().any(|inner| inner.contains(self.player_pos)))
            .map(|i| self.pending_announcements.remove(i))
            .map(|(_rect, msg)| msg.to_string())
    }

    pub fn player_pos(&self) -> Pos {
        self.player_pos
    }

    pub fn path(
        &self,
        start: Pos,
        end: Pos,
        maxdist: usize,
        through_walls: bool,
        around_mobs: bool,
    ) -> Option<Offset> {
        if start == end {
            return Some(Offset { x: 0, y: 0 });
        }
        let mut visited = HashSet::new();
        let mut periphery = Vec::new();
        let mut new_periphery = Vec::new();
        visited.insert(start);
        periphery.push(vec![start]);
        let mut closest_path: Option<Vec<_>> = None;
        loop {
            if periphery.is_empty() || periphery[0].len() > maxdist {
                return if let Some(ref p) = closest_path {
                    if p.len() >= 2 {
                        Some(p[1] - p[0])
                    } else {
                        None
                    }
                } else {
                    None
                };
            }
            if periphery[0].len() > maxdist {}
            for path in periphery.drain(..) {
                let pos = *path.last().unwrap();
                let adjacent = CARDINALS
                    .iter()
                    .copied()
                    .map(|c| pos + c)
                    .filter(|pos| !visited.contains(pos))
                    .filter(|pos| through_walls || self[*pos].kind.is_walkable())
                    .filter(|pos| !around_mobs || !self.mobs.contains_key(pos))
                    .collect::<Vec<_>>();
                for pos in adjacent {
                    visited.insert(pos);
                    let mut new_path = path.clone();
                    new_path.push(pos);
                    if pos == end {
                        return Some(new_path[1] - new_path[0]);
                    }
                    match closest_path {
                        None => {
                            closest_path = Some(new_path.clone());
                        }
                        Some(ref mut p) => {
                            if (pos - end).mhn_dist() < (*p.last().unwrap() - end).mhn_dist() {
                                *p = new_path.clone();
                            }
                        }
                    }
                    new_periphery.push(new_path);
                }
            }
            std::mem::swap(&mut periphery, &mut new_periphery);
            new_periphery.clear();
        }
    }
}

pub enum MissionState {
    Start,
    CodeEntered { seconds_left: u32 },
    Win,
}

pub struct GameState {
    pub world: World,
    pub player_memory: World,
    pub debug_mode: bool,
    pub state: MissionState,

    pub most_recent_mob: Option<MobKind>,
    pub announcements: VecDeque<String>,
}

impl GameState {
    pub fn new() -> GameState {
        GameState {
            world: World::new(&WALL_CHUNK),
            player_memory: World::new(&UNSEEN_CHUNK),
            debug_mode: false,
            state: MissionState::Start,

            most_recent_mob: None,
            announcements: VecDeque::new(),
        }
    }

    pub fn player_is_dead(&self) -> bool {
        self.world.player_damage >= PLAYER_MAX_HEALTH && !self.debug_mode
    }

    pub fn get_mob_text(&self, rng: &mut impl Rng) -> String {
        let visibility = fov::calculate_fov(self.world.player_pos, FOV_RANGE, &self.world);
        return self
            .world
            .mobs
            .iter()
            .filter(|(pos, _)| visibility.contains(pos))
            .map(|(pos, mob)| ((self.world.player_pos - *pos).mhn_dist(), mob))
            .min_by_key(|(dist, _)| *dist)
            .map(|mob| mob.1.kind.mob_description(rng))
            .unwrap_or_else(String::new);
    }

    pub fn generate_world(&mut self, seed: u64) {
        map_gen::generate_world(&mut self.world, seed);
        self.update_memory();
    }

    pub fn move_player(&mut self, o: Offset) -> bool {
        let force = self.debug_mode
            || (matches!(self.state, MissionState::CodeEntered { .. })
                && self.world[self.world.player_pos + o].kind == TileKind::Ocean);
        let ret = self.world.move_player(o, force);
        self.update_memory();
        ret
    }

    pub fn pick_up_item(&mut self) -> Option<Item> {
        let pos = self.world.player_pos;
        if let Some(item) = self.world[pos].item.take() {
            match item {
                Item::Ammo => {
                    self.world.player_ammo += 8;
                    Some(Item::Ammo)
                }
                other => {
                    // put it back
                    self.world[pos].item = Some(other);
                    None
                }
            }
        } else {
            None
        }
    }

    pub fn tick(&mut self, dt: f32, player_moved: bool, rng: &mut impl Rng) -> Vec<(Pos, Effect)> {
        let effects = self.world.tick(dt, player_moved, rng);
        if player_moved {
            self.state = match self.state {
                MissionState::Start => {
                    if self.world[self.world.player_pos].kind == TileKind::Computer {
                        MissionState::CodeEntered { seconds_left: 360 }
                    } else {
                        MissionState::Start
                    }
                }
                MissionState::CodeEntered { mut seconds_left } => {
                    if self.world[self.world.player_pos].kind == TileKind::Ocean {
                        MissionState::Win
                    } else {
                        if seconds_left != 0 {
                            seconds_left -= 1;
                            if seconds_left == 0 {
                                let player_pos = self.world.player_pos;
                                map_gen::carve_floor(
                                    &mut self.world,
                                    player_pos,
                                    20,
                                    TileKind::Fire,
                                );
                                self.world.player_damage += 1000;
                            }
                        }
                        MissionState::CodeEntered { seconds_left }
                    }
                }
                MissionState::Win => MissionState::Win,
            };

            if let Some(msg) = self.world.get_announcement() {
                self.announcements.push_back(msg);
                self.announcements.push_back(String::from("\n"));
            }

            if rng.gen::<f32>() < 0.01 {
                self.announcements.push_back(String::from(
                    *EMERGENCY_ANNOUNCEMENTS.choose(rng).unwrap(),
                ));
                self.announcements.push_back(String::from("\n"));
            }

            self.announcements = self
                .announcements
                .drain(..)
                .rev()
                .take(8)
                .rev()
                .collect::<VecDeque<_>>();
        }
        self.update_memory();

        effects
    }

    fn update_memory(&mut self) {
        let seen = fov::calculate_fov(self.world.player_pos, FOV_RANGE, &self.world);
        self.player_memory.mobs.clear();
        for pos in seen {
            self.player_memory[pos] = self.world[pos];
            if let Some(mob) = self.world.mobs.get(&pos) {
                self.player_memory.mobs.insert(pos, mob.clone());
            }
        }
        if self.debug_mode {
            self.player_memory
                .mobs
                .extend(self.world.mobs.iter().map(|(pos, mob)| (*pos, mob.clone())));
        }
    }
}
