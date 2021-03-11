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
}

impl Mob {
    fn new(kind: MobKind) -> Self {
        Self { kind, damage: 0 }
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

impl World {
    fn new(default_chunk: &'static Chunk) -> Self {
        let mut mobs = HashMap::new();
        mobs.insert(Pos { x: 1, y: 1 }, Mob::new(MobKind::Zombie));
        World {
            chunks: IndexMap::new(),
            default_chunk,
            mobs,
            player_pos: Pos { x: 0, y: 0 },
        }
    }

    pub fn move_player(&mut self, offset: Offset, force: bool) -> bool {
        let new_pos = self.player_pos + offset;
        if self[new_pos].kind.is_walkable() || force {
            self.player_pos += offset;
            true
        } else {
            false
        }
    }

    pub fn num_chunks(&self) -> usize {
        self.chunks.len()
    }

    pub fn carve_floor(&mut self, pos: Pos, brush_size: u8) {
        let brush_size = brush_size as i32;
        let brush_floor = -brush_size / 2;
        let brush_ceil = brush_floor + brush_size;
        for dx in brush_floor..=brush_ceil {
            for dy in brush_floor..=brush_ceil {
                self[pos + Offset { x: dx, y: dy }].kind = TileKind::Floor;
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

    pub fn carve_line(&mut self, start: Pos, end: Pos, brush_size: u8) {
        // based on https://www.redblobgames.com/grids/line-drawing.html (2.1)
        self.carve_floor(start, brush_size);
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
            self.carve_floor(pos, brush_size);
        }
    }

    pub fn carve_line_drunk(
        &mut self,
        start: Pos,
        end: Pos,
        brush_size: u8,
        rng: &mut impl Rng,
        waviness: f64,
    ) {
        let mut pos = start;
        while pos != end {
            let dir = if rng.gen::<f64>() < waviness {
                *DIRECTIONS.choose(rng).unwrap()
            } else {
                (end - pos).closest_dir()
            };
            pos += dir;
            self.carve_floor(pos, brush_size);
        }
    }

    fn update_mobs(&mut self) {
        let poses = self.mobs.keys().copied().collect::<Vec<_>>();
        for pos in poses {
            let mob = self.mobs.remove(&pos).unwrap();
            match mob.kind {
                MobKind::Zombie => {
                    if let Some(off) = self.path(pos, self.player_pos) {
                        let new_pos = pos + off;
                        if !self.mobs.contains_key(&new_pos) && self.player_pos != new_pos {
                            self.mobs.insert(new_pos, mob);
                        } else {
                            self.mobs.insert(pos, mob);
                        }
                    }
                }
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

    pub fn path(&self, start: Pos, end: Pos) -> Option<Offset> {
        if start == end {
            return Some(Offset { x: 0, y: 0 });
        }
        let mut visited = HashSet::new();
        let mut periphery = HashSet::new();
        let mut new_periphery = HashSet::new();
        visited.insert(start);
        periphery.insert(vec![start]);
        loop {
            if periphery.is_empty() {
                return None;
            }
            for path in periphery.drain() {
                let pos = *path.last().unwrap();
                visited.insert(pos);
                for pos in CARDINALS
                    .iter()
                    .copied()
                    .map(|c| pos + c)
                    .filter(|pos| !visited.contains(pos))
                {
                    let mut new_path = path.clone();
                    new_path.push(pos);
                    if pos == end {
                        return Some(new_path[1] - new_path[0]);
                    }
                    new_periphery.insert(new_path);
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
    world.carve_line_drunk(start, end, brush_size, &mut rng, 0.90);
}

pub struct GameState {
    pub world: World,
    pub player_memory: World,
    pub debug_mode: bool,
}

impl GameState {
    pub fn new() -> GameState {
        GameState {
            world: World::new(&WALL_CHUNK),
            player_memory: World::new(&UNSEEN_CHUNK),
            debug_mode: false,
        }
    }

    pub fn generate_world(&mut self, seed: u64) {
        generate_world(&mut self.world, seed);
        self.update_memory();
    }

    pub fn move_player(&mut self, o: Offset) -> bool {
        let ret = self.world.move_player(o, self.debug_mode);
        self.update_memory();
        ret
    }

    pub fn tick(&mut self, dt: f32, player_moved: bool) {
        self.world.tick(dt, player_moved);
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
    }
}
