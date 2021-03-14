use enum_map::{enum_map, Enum, EnumMap};
use indexmap::map::IndexMap;
use lazy_static::lazy_static;
use rand::Rng;
use rand::{seq::SliceRandom, SeedableRng};
use std::collections::HashMap;
use std::collections::HashSet;
use std::f64::consts::PI;
use std::ops::Div;
use std::ops::Sub;
use std::ops::{Add, AddAssign, Index, IndexMut, Mul};

use crate::fov;
use crate::map_gen;

pub const CHUNKSIZE: usize = 16;
pub const FOV_RANGE: i32 = 8;
pub const PLAYER_MAX_HEALTH: i32 = 4;

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
        let octant = (8f64 * angle / (2f64 * PI) + 8f64) as usize % 8;
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

impl MobKind {
    fn max_health(&self) -> u32 {
        match self {
            Self::Zombie => 2,
            Self::OldMan => 10,
            Self::Alien => 1,
            Self::Sculpture => 99,
        }
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
pub struct World {
    chunks: IndexMap<ChunkIndex, Chunk>,
    default_chunk: &'static Chunk,
    pub mobs: HashMap<Pos, Mob>,
    player_pos: Pos,
    player_damage: i32,
    pub player_ammo: u32,
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

#[derive(Clone, Copy, Debug)]
pub struct Rect {
    pub x1: i32,
    pub y1: i32,
    pub x2: i32,
    pub y2: i32,
}

impl Rect {
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

    pub fn num_chunks(&self) -> usize {
        self.chunks.len()
    }

    fn damage_mob(&mut self, pos: Pos) {
        self[pos].blood = true;
        let mob = self.mobs.get_mut(&pos).unwrap();
        mob.damage += 1;
        if mob.damage >= mob.kind.max_health() {
            drop(mob);
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
    ) -> Pos {
        let off = if through_walls {
            let mut off = (target - pos).norm();
            if off.x != 0 && off.y != 0 {
                // meh just bias y like the pathing
                off.x = 0;
            }
            Some(off)
        } else {
            self.path(pos, target, FOV_RANGE as usize * 3, around_mobs)
        };
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
        let never_saw_player_before = mob.saw_player_at.is_none();
        if !never_saw_player_before {
            if let Some(target) = mob.saw_player_at {
                if target == self.player_pos && (target - pos).mhn_dist() <= 1 {
                    self.player_damage += 1;
                    Some(pos)
                } else {
                    Some(self.move_towards(pos, target, through_walls, around_mobs))
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
            self.move_towards(pos, goal, true, true)
        } else {
            pos
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
                        for _ in 0..5 {
                            let next_pos = self
                                .pursue_if_seen_player(&mut mob, new_pos, false, true)
                                .unwrap_or_else(|| self.patrol(&mut mob, new_pos));
                            if seen.contains(&next_pos) {
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

    pub fn tick(&mut self, _dt: f32, player_moved: bool, rng: &mut impl Rng) -> Vec<(Pos, Effect)> {
        let mut effects = Vec::new();
        if player_moved {
            self.update_mobs(&mut effects, rng);
        }
        effects
    }

    pub fn player_pos(&self) -> Pos {
        self.player_pos
    }

    pub fn path(&self, start: Pos, end: Pos, maxdist: usize, around_mobs: bool) -> Option<Offset> {
        if start == end {
            return Some(Offset { x: 0, y: 0 });
        }
        let mut visited = HashSet::new();
        let mut periphery = Vec::new();
        let mut new_periphery = Vec::new();
        visited.insert(start);
        periphery.push(vec![start]);
        loop {
            if periphery.is_empty() {
                return None;
            }
            if periphery[0].len() > maxdist {
                return None;
            }
            for path in periphery.drain(..) {
                let pos = *path.last().unwrap();
                let adjacent = CARDINALS
                    .iter()
                    .copied()
                    .map(|c| pos + c)
                    .filter(|pos| !visited.contains(pos))
                    .filter(|pos| self[*pos].kind.is_walkable())
                    .filter(|pos| !around_mobs || !self.mobs.contains_key(pos))
                    .collect::<Vec<_>>();
                for pos in adjacent {
                    visited.insert(pos);
                    let mut new_path = path.clone();
                    new_path.push(pos);
                    if pos == end {
                        return Some(new_path[1] - new_path[0]);
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
}

impl GameState {
    pub fn new() -> GameState {
        GameState {
            world: World::new(&WALL_CHUNK),
            player_memory: World::new(&UNSEEN_CHUNK),
            debug_mode: false,
            state: MissionState::Start,
        }
    }

    pub fn player_is_dead(&self) -> bool {
        self.world.player_damage >= PLAYER_MAX_HEALTH && !self.debug_mode
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
            self.player_memory.mobs.extend(
                self.world
                    .mobs
                    .iter()
                    .map(|(pos, mob)| (pos.clone(), mob.clone())),
            );
        }
    }
}
