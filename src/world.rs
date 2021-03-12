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

pub const CHUNKSIZE: usize = 16;
pub const FOV_RANGE: i32 = 8;
pub const PLAYER_MAX_HEALTH: u32 = 4;

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

macro_rules! avg {
    ($n: expr, $d: expr) => {
        ($n + $d) / 2
    };
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Pos {
    pub x: i32,
    pub y: i32,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Offset {
    pub x: i32,
    pub y: i32,
}

impl Offset {
    pub fn diag_dist(self) -> u64 {
        self.x.abs().max(self.y.abs()) as u64
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
}

impl MobKind {
    fn max_health(&self) -> u32 {
        match self {
            Self::Zombie => 2,
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
        TileKind::Ocean=> TileKindInfo {
            opaque: false,
            walkable: false,
        },
        TileKind::BlackFloor=> TileKindInfo {
            opaque: false,
            walkable: true,
        },
        TileKind::YellowFloor=> TileKindInfo {
            opaque: false,
            walkable: true,
        },
        TileKind::BloodyFloor=> TileKindInfo {
            opaque: false,
            walkable: true,
        },
        TileKind::Computer=> TileKindInfo {
            opaque: true,
            walkable: true,
        },
        TileKind::Fire=> TileKindInfo {
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
pub struct Tile {
    pub kind: TileKind,
}

#[derive(Hash, Debug, Clone, Copy)]
pub struct Mob {
    pub kind: MobKind,
    pub damage: u32,
    pub saw_player_at: Option<Pos>,
}

impl Mob {
    fn new(kind: MobKind) -> Self {
        Self {
            kind,
            damage: 0,
            saw_player_at: None,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct Chunk {
    grid: [[Tile; CHUNKSIZE]; CHUNKSIZE],
}

const WALL_CHUNK: Chunk = Chunk {
    grid: [[Tile {
        kind: TileKind::Wall,
    }; CHUNKSIZE]; CHUNKSIZE],
};

const UNSEEN_CHUNK: Chunk = Chunk {
    grid: [[Tile {
        kind: TileKind::Unseen,
    }; CHUNKSIZE]; CHUNKSIZE],
};

#[derive(Debug, Clone)]
pub struct World {
    chunks: IndexMap<ChunkIndex, Chunk>,
    default_chunk: &'static Chunk,
    pub mobs: HashMap<Pos, Mob>,
    player_pos: Pos,
    player_damage: u32,
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

pub struct CarveRoomOpts {
    wall: TileKind,
    floor: TileKind,
    max_width: i32,
    max_height: i32,
    min_width: i32,
    min_height: i32,
}

#[derive(Clone, Copy)]
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
}

impl World {
    fn new(default_chunk: &'static Chunk) -> Self {
        let mut mobs = HashMap::new();
        mobs.insert(Pos { x: 1, y: 1 }, Mob::new(MobKind::Zombie));
        World {
            chunks: IndexMap::new(),
            default_chunk,
            mobs,
            player_pos: Pos { x: 0, y: 0 },
            player_damage: 0,
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

    pub fn carve_floor(&mut self, pos: Pos, brush_size: u8, tile: TileKind) {
        let brush_size = brush_size as i32;
        let brush_floor = -brush_size / 2;
        let brush_ceil = brush_floor + brush_size;
        for dx in brush_floor..=brush_ceil {
            for dy in brush_floor..=brush_ceil {
                self[pos + Offset { x: dx, y: dy }].kind = tile;
            }
        }
    }

    fn damage_mob(&mut self, pos: Pos) {
        let mob = self.mobs.get_mut(&pos).unwrap();
        mob.damage += 1;
        if mob.damage >= mob.kind.max_health() {
            drop(mob);
            self.mobs.remove(&pos);
        }
    }

    fn damage_player(&mut self) {
        self.player_damage += 1;
    }

    pub fn player_damage(&self) -> u32 {
        self.player_damage
    }

    pub fn fire(&mut self, start: Pos, off: Offset) -> Pos {
        let mut pos = start;
        loop {
            pos += off;
            if !TILE_INFOS[self[pos].kind].walkable {
                break pos;
            } else if self.mobs.get_mut(&pos).is_some() {
                self.damage_mob(pos);
                break pos;
            }
        }
    }

    pub fn carve_line(&mut self, start: Pos, end: Pos, brush_size: u8, tile: TileKind) {
        // based on https://www.redblobgames.com/grids/line-drawing.html (2.1)
        self.carve_floor(start, brush_size, tile);
        let mut pos = start;
        let offset = end - start;
        let (nx, ny) = (offset.x.abs(), offset.y.abs());
        let (mut ix, mut iy) = (0, 0);
        while (ix, iy) != (nx, ny) {
            if (1 + 2 * ix) * ny < (1 + 2 * iy) * nx {
                pos.x += offset.x.signum();
                ix += 1;
            } else {
                pos.y += offset.y.signum();
                iy += 1;
            }
            self.carve_floor(pos, brush_size, tile);
        }
    }

    pub fn carve_rect(&mut self, startx: i32, endx: i32, starty: i32, endy: i32, kind: TileKind) {
        for x in startx..=endx {
            for y in starty..=endy {
                let pos = Pos { x, y };
                self[pos].kind = kind;
            }
        }
    }

    pub fn carve_box(&mut self, startx: i32, endx: i32, starty: i32, endy: i32, kind: TileKind) {
        for x in startx..=endx {
            for &y in &[starty, endy] {
                let pos = Pos { x, y };
                self[pos].kind = kind;
            }
        }
        for y in starty..=endy {
            for &x in &[startx, endx] {
                let pos = Pos { x, y };
                self[pos].kind = kind;
            }
        }
    }
    pub fn carve_rooms_bsp_extra_loops(
        &mut self,
        startx: i32,
        endx: i32,
        starty: i32,
        endy: i32,
        opts: &CarveRoomOpts,
        rng: &mut impl Rng,
        loopiness: u32,
    ) -> Vec<Rect> {
        let rooms = self.carve_rooms_bsp(startx, endx, starty, endy, opts, rng);
        for _ in 0..loopiness {
            loop {
                let room1 = rooms.choose(rng).unwrap();
                let room2 = rooms.choose(rng).unwrap();
                if let Some(wall) = Self::get_connecting_wall(*room1, *room2) {
                    let pos = wall.choose(rng);
                    self.carve_floor(pos, 0, opts.floor);
                    break;
                }
            }
        }
        rooms
    }

    fn get_connecting_wall(room1: Rect, room2: Rect) -> Option<Rect> {
        // one-tile-wall between them
        for (room1, room2) in &[(room1, room2), (room2, room1)] {
            // room2 right of room1
            if room1.x2 + 2 == room2.x1 {
                let y1 = room1.y1.max(room2.y1);
                let y2 = room1.y2.min(room2.y2);
                if y1 <= y2 {
                    return Some(Rect {
                        x1: room1.x2 + 1,
                        x2: room1.x2 + 1,
                        y1,
                        y2,
                    });
                }
            }
            // room2 under room1
            if room1.y2 + 2 == room2.y1 {
                let x1 = room1.x1.max(room2.x1);
                let x2 = room1.x2.min(room2.x2);
                if x1 <= x2 {
                    return Some(Rect {
                        x1,
                        x2,
                        y1: room1.y2 + 1,
                        y2: room1.y2 + 1,
                    });
                }
            }
        }
        None
    }

    pub fn carve_rooms_bsp(
        &mut self,
        startx: i32,
        endx: i32,
        starty: i32,
        endy: i32,
        opts: &CarveRoomOpts,
        rng: &mut impl Rng,
    ) -> Vec<Rect> {
        assert!(opts.min_width * 2 + 1 < opts.max_width);
        assert!(opts.min_height * 2 + 1 < opts.max_height);
        #[derive(Clone, Copy, Debug)]
        enum Split {
            X,
            Y,
            None,
        };
        let too_wide = (endx - startx) > opts.max_width;
        let too_tall = (endy - starty) > opts.max_height;
        let split = match (too_wide, too_tall) {
            (true, true) => *[Split::X, Split::Y].choose(rng).unwrap(),
            (true, false) => Split::X,
            (false, true) => Split::Y,
            _ => Split::None,
        };
        match split {
            Split::X => {
                let split_x =
                    rng.gen_range(startx + opts.min_width + 1..(endx - opts.min_width - 1));
                let mut rooms = self.carve_rooms_bsp(startx, split_x - 1, starty, endy, opts, rng);
                rooms.extend(self.carve_rooms_bsp(split_x + 1, endx, starty, endy, opts, rng));
                let mid_left = Pos {
                    x: avg!(startx, split_x - 1),
                    y: avg!(starty, endy),
                };
                let mid_right = Pos {
                    x: avg!(split_x + 1, endx),
                    y: avg!(starty, endy),
                };
                self.carve_line(mid_left, mid_right, 0, opts.floor);
                rooms
            }
            Split::Y => {
                let split_y = rng.gen_range(starty + opts.min_height + 1..(endy - opts.min_height));
                let mut rooms = self.carve_rooms_bsp(startx, endx, starty, split_y - 1, opts, rng);
                rooms.extend(self.carve_rooms_bsp(startx, endx, split_y + 1, endy, opts, rng));
                let mid_top = Pos {
                    x: avg!(startx, endx),
                    y: avg!(starty, split_y - 1),
                };
                let mid_bot = Pos {
                    x: avg!(startx, endx),
                    y: avg!(split_y + 1, endy),
                };
                self.carve_line(mid_top, mid_bot, 0, opts.floor);
                rooms
            }
            Split::None => {
                // just carve the room
                self.carve_rect(startx, endx, starty, endy, opts.floor);
                vec![Rect {
                    x1: startx,
                    x2: endx,
                    y1: starty,
                    y2: endy,
                }]
            }
        }
    }

    pub fn carve_line_drunk(
        &mut self,
        start: Pos,
        end: Pos,
        brush_size: u8,
        rng: &mut impl Rng,
        waviness: f64,
        tile: TileKind,
    ) {
        let mut pos = start;
        while pos != end {
            let dir = if rng.gen::<f64>() < waviness {
                *DIRECTIONS.choose(rng).unwrap()
            } else {
                (end - pos).closest_dir()
            };
            pos += dir;
            self.carve_floor(pos, brush_size, tile);
        }
    }

    fn update_mobs(&mut self) {
        let seen = fov::calculate_fov(self.player_pos, FOV_RANGE, &self);
        let poses = self.mobs.keys().copied().collect::<Vec<_>>();
        for pos in poses {
            let mut mob = self.mobs.remove(&pos).unwrap();
            let new_pos = match mob.kind {
                MobKind::Zombie => {
                    let never_saw_player_before = mob.saw_player_at.is_none();
                    if seen.contains(&pos) {
                        mob.saw_player_at = Some(self.player_pos);
                    }
                    if !never_saw_player_before {
                        if let Some(target) = mob.saw_player_at {
                            if let Some(off) = self.path(pos, target, 20) {
                                let new_pos = pos + off;
                                if self.player_pos == new_pos {
                                    self.player_damage += 1;
                                    Some(pos)
                                } else if !self.mobs.contains_key(&new_pos) {
                                    Some(new_pos)
                                } else {
                                    Some(pos)
                                }
                            } else {
                                Some(pos)
                            }
                        } else {
                            Some(pos)
                        }
                    } else {
                        Some(pos)
                    }
                }
            };
            if let Some(pos) = new_pos {
                self.mobs.insert(pos, mob);
            }
        }
    }

    pub fn tick(&mut self, _dt: f32, player_moved: bool) {
        if player_moved {
            self.update_mobs();
        }
    }

    pub fn player_pos(&self) -> Pos {
        self.player_pos
    }

    pub fn path(&self, start: Pos, end: Pos, maxdist: usize) -> Option<Offset> {
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

pub fn generate_world(world: &mut World, seed: u64) {
    let mut rng = rand::rngs::SmallRng::seed_from_u64(seed);
    let start = Pos { x: 0, y: 0 };
    let end = Pos { x: 900, y: 0 };
    let brush_size = 2;
    world[start].kind = TileKind::Floor;
    // left ocean
    world.carve_rect(-20, 40, -30, 30, TileKind::Ocean);
    world.carve_rect(-10, 10, -10, 10, TileKind::BlackFloor);
    // h for helicopter
    world.carve_rect(-3, -3, -3, 3, TileKind::YellowFloor);
    world.carve_rect(3, 3, -3, 3, TileKind::YellowFloor);
    world.carve_rect(-3, 3, 0, 0, TileKind::YellowFloor);
    // facility
    world.carve_rect(8, 100, -30, 30, TileKind::Wall);
    let bsp_opts = CarveRoomOpts {
        wall: TileKind::Wall,
        floor: TileKind::Floor,
        max_width: 10,
        max_height: 10,
        min_width: 2,
        min_height: 2,
    };
    let rooms = world.carve_rooms_bsp_extra_loops(9, 99, -29, 29, &bsp_opts, &mut rng, 300);
    for room in &rooms {
        // furnish the rooms a little
        let r: f32 = rng.gen_range(0.0..1.0);
        if r < 0.30 {
            for _ in 0..10 {
                // add some spashes of blood
                let x1 = rng.gen_range(room.x1..=room.x2);
                let x2 = rng.gen_range(room.x1..=room.x2);
                let y1 = rng.gen_range(room.y1..=room.y2);
                let y2 = rng.gen_range(room.y1..=room.y2);
                if x1 < x2 && y1 < y2 {
                    world.carve_rect(x1, x2, y1, y2, TileKind::BloodyFloor);
                }
            }
        }
    }
    // spawn some enemies
    for _ in 0..100 {
        let room = rooms.choose(&mut rng).unwrap();
        let x = rng.gen_range(room.x1..=room.x2);
        let y = rng.gen_range(room.y1..=room.y2);
        let pos = Pos { x, y };
        world.mobs.insert(pos, Mob::new(MobKind::Zombie));
    }
    world.carve_floor(Pos { x: 8, y: 0}, 1, TileKind::Floor);

    let rightmost_room = **rooms
        .iter()
        .filter(|r| r.x2 == 99)
        .collect::<Vec<_>>()
        .choose(&mut rng)
        .unwrap();
    let center_y = avg!(rightmost_room.y1, rightmost_room.y2);
    let right_wall = Pos {
        x: 100,
        y: center_y,
    };
    world.carve_floor(right_wall, 0, TileKind::Floor);
    world.carve_rect(
        right_wall.x + 1,
        right_wall.x + 4,
        right_wall.y - 2,
        right_wall.y + 2,
        TileKind::BloodyFloor,
    );
    // goal
    world.carve_floor(right_wall + Offset { x: 2, y: 0 }, 0, TileKind::Computer);
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
        generate_world(&mut self.world, seed);
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

    pub fn tick(&mut self, dt: f32, player_moved: bool) {
        self.world.tick(dt, player_moved);
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
                                self.world
                                    .carve_floor(self.world.player_pos, 20, TileKind::Fire);
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
    }

    fn update_memory(&mut self) {
        let seen = fov::calculate_fov(self.world.player_pos, FOV_RANGE, &self.world);
        self.player_memory.mobs.clear();
        for pos in seen {
            self.player_memory[pos] = self.world[pos];
            if let Some(mob) = self.world.mobs.get(&pos) {
                self.player_memory.mobs.insert(pos, *mob);
            }
        }
        if self.debug_mode {
            self.player_memory.mobs.extend(&self.world.mobs);
        }
    }
}
