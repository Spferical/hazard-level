use crate::world::CARDINALS;
use std::collections::HashMap;
use std::io::Cursor;

use rand::rngs::SmallRng;
use rand::Rng;
use rand::{seq::SliceRandom, SeedableRng};

use image;

use crate::world::{Item, Mob, MobKind, Offset, Pos, Rect, TileKind, World, DIRECTIONS};

pub const SEGMENTS: &[(&[u8], (u32, u32))] = &[
    (include_bytes!("../static/0.png"), (10, 10)),
    (include_bytes!("../static/1.png"), (18, 18)), // make more stuff
    (include_bytes!("../static/0.png"), (10, 10)),
    (include_bytes!("../static/1.png"), (18, 18)),
];

macro_rules! avg {
    ($n: expr, $d: expr) => {
        ($n + $d) / 2
    };
}

#[derive(Debug, Clone, Copy)]
pub struct CarveRoomOpts {
    wall: TileKind,
    floor: TileKind,
    max_width: i32,
    max_height: i32,
    min_width: i32,
    min_height: i32,
}

impl From<CarveRoomOpts> for BspSplitOpts {
    fn from(opts: CarveRoomOpts) -> Self {
        Self {
            max_width: opts.max_width,
            max_height: opts.max_height,
            min_width: opts.min_width,
            min_height: opts.min_height,
        }
    }
}

pub fn carve_room(
    world: &mut World,
    room: Rect,
    adj_rooms: Vec<Rect>,
    rng: &mut impl Rng,
    floor: TileKind,
) {
    fill_rect(world, room, floor);
    for adj in adj_rooms {
        let wall = get_connecting_wall(room, adj).unwrap();
        let has_door = wall.into_iter().any(|pos| world[pos].kind.is_walkable());
        if !has_door {
            carve_floor(world, wall.choose(rng), 0, floor);
        }
    }
}

pub fn carve_rooms_bsp(
    world: &mut World,
    rect: Rect,
    opts: &CarveRoomOpts,
    rng: &mut impl Rng,
) -> Vec<Rect> {
    let tree = gen_bsp_tree(rect, (*opts).into(), rng);
    let room_graph = tree.into_room_graph();
    for room in room_graph.iter() {
        fill_rect(world, room, opts.floor);
        for adj in room_graph.get_adj(room).unwrap() {
            let wall = get_connecting_wall(room, *adj).unwrap();
            let has_door = wall.into_iter().any(|pos| world[pos].kind.is_walkable());
            if !has_door {
                carve_floor(world, wall.choose(rng), 0, opts.floor);
            }
        }
    }
    room_graph.iter().collect()
}

pub fn carve_rooms_bsp_extra_loops(
    world: &mut World,
    rect: Rect,
    opts: &CarveRoomOpts,
    rng: &mut impl Rng,
    loopiness: f32,
) -> Vec<Rect> {
    let rooms = carve_rooms_bsp(world, rect, opts, rng);
    for _ in 0..((rooms.len() - 1) as f32 * loopiness) as u32 {
        loop {
            let room1 = rooms.choose(rng).unwrap();
            let room2 = rooms.choose(rng).unwrap();
            if let Some(wall) = get_connecting_wall(*room1, *room2) {
                let pos = wall.choose(rng);
                carve_floor(world, pos, 0, opts.floor);
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

// given rect, get rooms this room can connect to
// returns vec of (connecting walls, other room)
fn get_adjacent(room: Rect, rooms: &Vec<Rect>) -> Vec<(Rect, Rect)> {
    let mut adj = Vec::new();
    for other in rooms.iter().cloned() {
        if let Some(wall) = get_connecting_wall(room, other) {
            adj.push((wall, other));
        }
    }
    adj
}

#[derive(Clone, Copy, Debug)]
pub struct BspSplitOpts {
    max_width: i32,
    max_height: i32,
    min_width: i32,
    min_height: i32,
}

pub enum BspTree {
    Split(Box<BspTree>, Box<BspTree>),
    Room(Rect),
}

impl BspTree {
    fn into_room_graph(self) -> RoomGraph {
        match self {
            BspTree::Room(rect) => {
                let mut graph = RoomGraph::new();
                graph.add_room(rect);
                graph
            }
            BspTree::Split(tree1, tree2) => {
                let mut rooms1 = tree1.into_room_graph();
                let rooms2 = tree2.into_room_graph();
                // now figure out how to bridge the trees
                rooms1.extend_bridged(rooms2);
                rooms1
            }
        }
    }
}

struct RoomGraph {
    pub room_adj: HashMap<Rect, Vec<Rect>>,
}

impl RoomGraph {
    fn get_adj(&self, rect: Rect) -> Option<&[Rect]> {
        self.room_adj.get(&rect).map(|v| v.as_slice())
    }
    fn choose(&self, rng: &mut impl Rng) -> Option<Rect> {
        if self.room_adj.is_empty() {
            return None;
        }
        let idx = rng.gen_range(0..self.room_adj.len());
        self.room_adj.keys().nth(idx).cloned()
    }
    fn len(&self) -> usize {
        self.room_adj.len()
    }
    fn is_connected(&self, rect1: Rect, rect2: Rect) -> bool {
        self.room_adj
            .get(&rect1)
            .map(|adj| adj.contains(&rect2))
            .unwrap_or(false)
    }
    fn remove_room(&mut self, rect: Rect) {
        self.room_adj.retain(|r, _| *r != rect);
    }
    fn find_spatially_adjacent(&self, rect: Rect) -> Option<Rect> {
        for (room, _adjs) in &self.room_adj {
            if let Some(_wall) = get_connecting_wall(rect, *room) {
                return Some(*room);
            }
        }
        None
    }
    fn extend_bridged(&mut self, mut other: RoomGraph) {
        let mut bridged = false;
        'loop1: for (room1, ref mut adj1) in &mut self.room_adj {
            for (room2, ref mut adj2) in &mut other.room_adj {
                if get_connecting_wall(*room1, *room2).is_some() {
                    bridged = true;
                    adj1.push(*room2);
                    adj2.push(*room1);
                    break 'loop1;
                }
            }
        }
        assert!(bridged);
        self.room_adj.extend(other.room_adj);
    }
    fn extend(&mut self, other: RoomGraph) {
        self.room_adj.extend(other.room_adj)
    }
    fn new() -> Self {
        Self {
            room_adj: HashMap::new(),
        }
    }
    fn add_room(&mut self, room: Rect) {
        self.room_adj.insert(room, vec![]);
    }
    fn add_connection(&mut self, room1: Rect, room2: Rect) {
        assert!(get_connecting_wall(room1, room2).is_some());
        assert!(self.room_adj.contains_key(&room1));
        assert!(self.room_adj.contains_key(&room2));
        self.room_adj.get_mut(&room2).unwrap().push(room1);
        self.room_adj.get_mut(&room1).unwrap().push(room2);
    }
    fn add_connection_oneway(&mut self, room1: Rect, room2: Rect) {
        assert!(get_connecting_wall(room1, room2).is_some());
        assert!(self.room_adj.contains_key(&room1));
        self.room_adj.get_mut(&room1).unwrap().push(room2);
    }

    fn iter<'a>(&'a self) -> impl Iterator<Item = Rect> + 'a {
        self.room_adj.keys().copied()
    }
}

// returns (rooms, walls between connected rooms in the bsp tree)
pub fn gen_bsp_tree(rect: Rect, opts: BspSplitOpts, rng: &mut impl Rng) -> BspTree {
    assert!(opts.min_width * 2 + 1 <= opts.max_width);
    assert!(opts.min_height * 2 + 1 <= opts.max_height);
    #[derive(Clone, Copy, Debug)]
    enum Split {
        X,
        Y,
        None,
    };
    let too_wide = (rect.x2 - rect.x1) > opts.max_width;
    let too_tall = (rect.y2 - rect.y1) > opts.max_height;
    let split = match (too_wide, too_tall) {
        (true, true) => *[Split::X, Split::Y].choose(rng).unwrap(),
        (true, false) => Split::X,
        (false, true) => Split::Y,
        _ => Split::None,
    };
    match split {
        Split::X => {
            let split_x =
                rng.gen_range(rect.x1 + opts.min_width + 1..(rect.x2 - opts.min_width - 1));
            let left = Rect::new(rect.x1, split_x - 1, rect.y1, rect.y2);
            let right = Rect::new(split_x + 1, rect.x2, rect.y1, rect.y2);
            BspTree::Split(
                Box::new(gen_bsp_tree(left, opts, rng)),
                Box::new(gen_bsp_tree(right, opts, rng)),
            )
        }
        Split::Y => {
            let split_y = rng.gen_range(rect.y1 + opts.min_height + 1..(rect.y2 - opts.min_height));
            let top = Rect::new(rect.x1, rect.x2, rect.y1, split_y - 1);
            let bottom = Rect::new(rect.x1, rect.x2, split_y + 1, rect.y2);
            BspTree::Split(
                Box::new(gen_bsp_tree(top, opts, rng)),
                Box::new(gen_bsp_tree(bottom, opts, rng)),
            )
        }
        Split::None => BspTree::Room(rect),
    }
}

pub fn carve_line_drunk(
    world: &mut World,
    start: Pos,
    end: Pos,
    brush_size: u8,
    rng: &mut impl Rng,
    waviness: f64,
    tile: TileKind,
    bound: Rect,
) {
    let mut pos = start;
    while pos != end {
        let dir = if rng.gen::<f64>() < waviness {
            *CARDINALS.choose(rng).unwrap()
        } else {
            (end - pos).closest_dir()
        };
        if !bound.contains(pos + dir) {
            continue;
        }
        pos += dir;
        carve_floor(world, pos, brush_size, tile);
    }
}

pub fn box_rect(world: &mut World, rect: Rect, kind: TileKind) {
    for x in rect.x1..=rect.x2 {
        for &y in &[rect.y1, rect.y2] {
            let pos = Pos { x, y };
            world[pos].kind = kind;
        }
    }
    for y in rect.y1..=rect.y2 {
        for &x in &[rect.x1, rect.x2] {
            let pos = Pos { x, y };
            world[pos].kind = kind;
        }
    }
}

pub fn carve_line(world: &mut World, start: Pos, end: Pos, brush_size: u8, tile: TileKind) {
    // based on https://www.redblobgames.com/grids/line-drawing.html (2.1)
    carve_floor(world, start, brush_size, tile);
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
        carve_floor(world, pos, brush_size, tile);
    }
}

pub fn fill_rect(world: &mut World, rect: Rect, kind: TileKind) {
    for x in rect.x1..=rect.x2 {
        for y in rect.y1..=rect.y2 {
            let pos = Pos { x, y };
            world[pos].kind = kind;
        }
    }
}

fn gen_alien_nest(world: &mut World, rng: &mut impl Rng, entrances: &[Pos], rect: Rect) {
    // draw a bunch of lines between the entrances
    let mut interior_entrances = Vec::new();
    for &e in entrances {
        for &o in &CARDINALS {
            if rect.contains(e + o) {
                interior_entrances.push(e + o);
            }
        }
    }
    fill_rect(world, rect, TileKind::YellowWall);
    // draw lines between interior entrances
    for &e1 in &interior_entrances {
        for &e2 in interior_entrances.iter().chain(&[rect.center()]) {
            carve_line_drunk(world, e1, e2, 0, rng, 0.5, TileKind::YellowFloor, rect);
        }
    }
    // spawn some enemies
    let size = rect.width() * rect.height();
    for _ in 0..(size / 20).max(1) {
        loop {
            let pos = rect.choose(rng);
            if !world[pos].kind.is_walkable() {
                continue;
            }
            let kind = MobKind::Alien;
            world.mobs.insert(pos, Mob::new(kind));
            break;
        }
    }

    // spawn some ammo
    for _ in 0..(size / 70).max(1) {
        loop {
            let pos = rect.choose(rng);
            if world[pos].kind.is_walkable() && world[pos].item.is_none() {
                world[pos].item = Some(Item::Ammo);
                break;
            }
        }
    }
}

fn gen_offices(world: &mut World, rng: &mut impl Rng, entrances: &[Pos], rect: Rect) {
    // offices
    for entrance in entrances {
        carve_floor(world, *entrance, 1, TileKind::Floor);
    }
    let max_width = rng.gen_range(4..=rect.width().min(8));
    let min_width = max_width / 2 - 1;
    let max_height = rng.gen_range(4..=rect.width().min(8));
    let min_height = max_height / 2 - 1;
    let bsp_opts = CarveRoomOpts {
        wall: TileKind::Wall,
        floor: TileKind::Floor,
        max_width,
        max_height,
        min_width,
        min_height,
    };
    // let rect = Rect::new(rect.x1 + 1, rect.x2 - 1, rect.y1 + 1, rect.y2 - 1);
    let rooms = carve_rooms_bsp_extra_loops(world, rect, &bsp_opts, rng, 1.0);
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
                    fill_rect(world, Rect { x1, x2, y1, y2 }, TileKind::BloodyFloor);
                }
            }
        }
    }
    let size = rect.width() * rect.height();
    // spawn some enemies
    for _ in 0..(size / 20).max(1) {
        let room = rooms.choose(rng).unwrap();
        let x = rng.gen_range(room.x1..=room.x2);
        let y = rng.gen_range(room.y1..=room.y2);
        let pos = Pos { x, y };
        let rand = rng.gen::<f32>();
        let kind = if rand < 0.01 {
            MobKind::Sculpture
        } else if rand < 0.1 {
            MobKind::Alien
        } else {
            MobKind::Zombie
        };
        world.mobs.insert(pos, Mob::new(kind));
    }

    // spawn some ammo
    for _ in 0..(size / 70).max(1) {
        loop {
            let room = rooms.choose(rng).unwrap();
            let pos = room.choose(rng);
            if world[pos].item.is_none() {
                world[pos].item = Some(Item::Ammo);
                break;
            }
        }
    }
    carve_floor(world, Pos { x: 8, y: 0 }, 1, TileKind::Floor);
}

fn generate_containment(
    world: &mut World,
    rng: &mut impl Rng,
    left_entrance: Pos,
) -> (Pos, Offset) {
    // predictable grid of corridors
    let corridor_width = 2;
    let room_size = 2;
    carve_floor(world, left_entrance, 1, TileKind::Floor);
    // assuming everything's walls already
    // corridors
    let corridors_vert = 3;
    let corridors_horiz = 4;
    let between_corridors = 3 + room_size * 2;
    let rect = Rect::new(
        left_entrance.x,
        left_entrance.x
            + corridors_vert * corridor_width
            + (corridors_vert - 1) * between_corridors
            - 1,
        left_entrance.y,
        left_entrance.y
            + corridors_horiz * corridor_width
            + (corridors_horiz - 1) * between_corridors
            - 1,
    );
    // horizontal
    for i in 0..corridors_horiz {
        let y1 = rect.y1 + i * (corridor_width + between_corridors);
        let y2 = y1 + corridor_width - 1;
        let corr_rect = Rect::new(rect.x1, rect.x2, y1, y2);
        fill_rect(world, corr_rect, TileKind::Floor);
    }
    // vertical
    for i in 0..corridors_vert {
        let x1 = rect.x1 + i * (corridor_width + between_corridors);
        let x2 = x1 + corridor_width - 1;
        let corr_rect = Rect::new(x1, x2, rect.y1, rect.y2);
        fill_rect(world, corr_rect, TileKind::Floor);
    }
    // rooms
    let mut rooms = Vec::<Rect>::new();
    for sq_x in 0..corridors_vert - 1 {
        for sq_y in 0..corridors_horiz - 1 {
            let x1 = rect.x1 + sq_x * (corridor_width + between_corridors) + corridor_width;
            let x2 = rect.x1 + (sq_x + 1) * (corridor_width + between_corridors) - 1;
            let y1 = rect.y1 + sq_y * (corridor_width + between_corridors) + corridor_width;
            let y2 = rect.y1 + (sq_y + 1) * (corridor_width + between_corridors) - 1;
            let block = [
                Rect::new(x1 + 1, x1 + room_size, y1 + 1, y1 + room_size),
                Rect::new(x1 + 1, x1 + room_size, y2 - room_size, y2 - 1),
                Rect::new(x2 - room_size, x2 - 1, y1 + 1, y1 + room_size),
                Rect::new(x2 - room_size, x2 - 1, y2 - room_size, y2 - 1),
            ];
            for room in &block {
                fill_rect(world, *room, TileKind::Floor);
            }
            carve_floor(world, Pos::new(x1 + room_size, y1), 0, TileKind::Floor);
            carve_floor(world, Pos::new(x2 - room_size, y1), 0, TileKind::Floor);
            carve_floor(world, Pos::new(x1 + room_size, y2), 0, TileKind::Floor);
            carve_floor(world, Pos::new(x2 - room_size, y2), 0, TileKind::Floor);
            rooms.extend(&block);
        }
    }
    let exit = Pos::new(rect.x2, avg!(rect.y1, rect.y2));

    let mut man = Mob::new(MobKind::OldMan);
    man.patrol = Some(vec![
        Pos::new(rect.x1 + 1, rect.y1 + 1),
        Pos::new(rect.x1 + 1, rect.y2 - 1),
        Pos::new(rect.x2 - 1, rect.y2 - 1),
        Pos::new(rect.x2 - 1, rect.y1 + 1),
    ]);
    world.mobs.insert(exit, man);

    world.thing.pos = exit + Offset { x: -1, y: 0 };

    // spawn some enemies
    for _ in 0..10 {
        let room = rooms.choose(rng).unwrap();
        let x = rng.gen_range(room.x1..=room.x2);
        let y = rng.gen_range(room.y1..=room.y2);
        let pos = Pos { x, y };
        let rand = rng.gen::<f32>();
        let kind = if rand < 0.1 {
            MobKind::Alien
        } else {
            MobKind::Zombie
        };
        world.mobs.insert(pos, Mob::new(kind));
    }

    // spawn some ammo
    for _ in 0..2 {
        loop {
            let room = rooms.choose(rng).unwrap();
            let pos = room.choose(rng);
            if world[pos].item.is_none() {
                world[pos].item = Some(Item::Ammo);
                break;
            }
        }
    }
    // spawn the thing

    (exit, Offset { x: 1, y: 0 })
}

pub fn sample_until_valid(segment_shape: (u32, u32), rng: &mut impl Rng) -> usize {
    loop {
        let id = rng.gen_range(0..SEGMENTS.len());
        if (SEGMENTS[id].1).0 < segment_shape.0 && (SEGMENTS[id].1).1 < segment_shape.1 {
            return id;
        }
    }
}

pub fn generate_world(world: &mut World, seed: u64) {
    let mut rng = SmallRng::seed_from_u64(seed);
    // left ocean none beef
    fill_rect(world, Rect::new(-50, 10, -50, 50), TileKind::Ocean);
    fill_rect(world, Rect::new(-10, 10, -10, 10), TileKind::BlackFloor);
    // h for helicopter
    fill_rect(world, Rect::new(-3, -3, -3, 3), TileKind::YellowFloor);
    fill_rect(world, Rect::new(3, 3, -3, 3), TileKind::YellowFloor);
    fill_rect(world, Rect::new(-3, 3, 0, 0), TileKind::YellowFloor);
    let start_room = Rect::new(-10, 10, -10, 10);

    let world_rect = Rect {
        x1: 11,
        x2: 111,
        y1: -50,
        y2: 50,
    };
    fill_rect(world, world_rect, TileKind::Wall);
    let world_rect = world_rect.expand(-1);
    let bsp_opts = BspSplitOpts {
        max_width: 36,
        max_height: 36,
        min_width: 15,
        min_height: 15,
    };
    let big_bsp = gen_bsp_tree(world_rect, bsp_opts, &mut rng);
    let mut room_graph = big_bsp.into_room_graph();

    // create the entrance
    let next_to_entrance = room_graph.find_spatially_adjacent(start_room).unwrap();
    let wall = get_connecting_wall(next_to_entrance, start_room).unwrap();
    fill_rect(world, wall, TileKind::BlackFloor);
    room_graph.add_connection_oneway(next_to_entrance, start_room);

    // pick the smallest right-most room and empty it and add a computer
    let final_room = room_graph
        .iter()
        .filter(|r| r.x2 == world_rect.x2)
        .min_by_key(|r| r.x2)
        .unwrap()
        .clone();
    room_graph.remove_room(final_room);
    fill_rect(world, final_room, TileKind::BloodyFloor);
    carve_floor(world, final_room.center(), 0, TileKind::Computer);
    world.thing.pos = final_room.center() + Offset { x: 1, y: 1 };

    // add some loops to the rooms
    let loopiness = 1.0;
    for _ in 0..((room_graph.len() - 1) as f32 * loopiness) as u32 {
        loop {
            let room1 = room_graph.choose(&mut rng).unwrap();
            let room2 = room_graph.choose(&mut rng).unwrap();
            if let Some(_wall) = get_connecting_wall(room1, room2) {
                room_graph.add_connection(room1, room2);
                break;
            }
        }
    }
    fn sort_rooms(room1: Rect, room2: Rect) -> (Rect, Rect) {
        let mut arr = [room1, room2];
        arr.sort_by_key(|r| (r.x1, r.y1));
        (arr[0], arr[1])
    }

    // decide on specific tile to be the entrance between each room pair
    let mut rooms_to_door = HashMap::new();
    for room1 in room_graph.iter() {
        for &room2 in room_graph.get_adj(room1).unwrap() {
            let (room1, room2) = sort_rooms(room1, room2);
            if !rooms_to_door.contains_key(&(room1, room2)) {
                let wall = get_connecting_wall(room1, room2).unwrap();
                let door = wall.choose(&mut rng);
                rooms_to_door.insert((room1, room2), door);
            }
        }
    }

    // Spferical's old map gen
    // finally, gen the intermediate rooms
    for room in room_graph.iter() {
        let adjs = room_graph.get_adj(room).unwrap();
        // carve_room(world, room, adjs, &mut rng, TileKind::Floor);
        let entrances = adjs
            .into_iter()
            .copied()
            .map(|adj| rooms_to_door.get(&sort_rooms(room, adj)).unwrap())
            .copied()
            .collect::<Vec<_>>();
        let rand = rng.gen::<f32>();

        let sample_id = sample_until_valid(
            ((room.y2 - room.y1) as u32, (room.x2 - room.x1) as u32),
            &mut rng,
        );

        if rand <= 0.9 {
            //gen_offices(world, &mut rng, &entrances, room);
            let nexuses = carve_segment(world, sample_id, room, TileKind::BlackFloor, &mut rng);

            for entrance in entrances {
                let closest_nexus = *nexuses
                    .iter()
                    .min_by_key(|&nexus| (*nexus - entrance).mhn_dist())
                    .unwrap();
                carve_line(world, entrance, closest_nexus, 0, TileKind::YellowFloor);
            }
        } else {
            gen_alien_nest(world, &mut rng, &entrances, room);
            for entrance in entrances {
                carve_floor(world, entrance, 0, TileKind::Floor);
            }
        }
    }

    // Animated's new map gen
    /*let mut rooms_to_nexuses = HashMap::new();
    for segment in room_graph.iter() {
        let sample_id = sample_until_valid(
            (
                (segment.y2 - segment.y1) as u32,
                (segment.x2 - segment.x1) as u32,
            ),
            &mut rng,
        );

        // carve out a given segment, and get the doors on it
        // TODO: Also add mobs to these areas
        let nexuses = carve_segment(world, sample_id, segment, TileKind::BlackFloor, &mut rng);

        rooms_to_nexuses.insert(segment, nexuses);
    }
    for segment in room_graph.iter() {
        // get slice of adjacent rooms to a given room
        let adjs = room_graph.get_adj(segment).unwrap();

        // connect all adjacent rooms
        for adj_segment in adjs {
            // TODO: We shouldn't need this check!
            if rooms_to_nexuses.contains_key(&adj_segment) {
                carve_corridor(
                    world,
                    segment,
                    *adj_segment,
                    &rooms_to_nexuses[&segment],
                    &rooms_to_nexuses[adj_segment],
                )
            }
        }
    }*/

    // add old man
    {
        let rect = world_rect.expand(-10);
        let mut man = Mob::new(MobKind::OldMan);
        man.patrol = Some(vec![
            rect.topleft(),
            rect.bottomleft(),
            rect.bottomright(),
            rect.topright(),
        ]);
        world.mobs.insert(rect.topleft(), man);
    }
}

pub fn carve_floor(world: &mut World, pos: Pos, brush_size: u8, tile: TileKind) {
    let brush_size = brush_size as i32;
    let brush_floor = -brush_size / 2;
    let brush_ceil = brush_floor + brush_size;
    for dx in brush_floor..=brush_ceil {
        for dy in brush_floor..=brush_ceil {
            world[pos + Offset { x: dx, y: dy }].kind = tile;
        }
    }
}

pub fn carve_segment(
    world: &mut World,
    segment_id: usize,
    segment_dimensions: Rect,
    floor_type: TileKind,
    rng: &mut impl Rng,
) -> Vec<Pos> {
    let (img_bytes, _) = SEGMENTS[segment_id];
    let img = image::io::Reader::new(Cursor::new(img_bytes))
        .with_guessed_format()
        .expect("unable to read png file in memory??")
        .decode()
        .unwrap();

    let room_width = (segment_dimensions.x2 - segment_dimensions.x1) as u32;
    let room_height = (segment_dimensions.y2 - segment_dimensions.y1) as u32;

    // random rotation
    let img = match rng.gen_range(0..4) {
        1 => image::imageops::rotate90(&img),
        2 => image::imageops::rotate180(&img),
        3 => image::imageops::rotate270(&img),
        _ => img.into_rgba8(),
    };

    // scale to size
    let img = image::imageops::resize(
        &img,
        room_width,
        room_height,
        image::imageops::FilterType::Nearest,
    );

    let mut nexuses: Vec<Pos> = Vec::new();

    // Carve out rooms
    for (x, y, pixel) in img.enumerate_pixels() {
        let alpha = pixel[0];
        let nexus = pixel[1];

        let pos = Pos::new(x as i32, y as i32)
            + Offset {
                x: segment_dimensions.x1,
                y: segment_dimensions.y1,
            };

        if nexus > 0 && alpha == 0 {
            world[pos].kind = floor_type;
            nexuses.push(pos);
        } else {
            // alpha is 0 if it is a wall, 255 if it is empty
            if rng.gen_range(0..255) < alpha {
                //if alpha > 0 {
                // we carve it out
                world[pos].kind = floor_type;
            }
        }
    }

    nexuses
}

pub fn carve_corridor(
    world: &mut World,
    segment_a: Rect,
    segment_b: Rect,
    a_doors: &Vec<Pos>,
    b_doors: &Vec<Pos>,
) {
    let connecting_wall = get_connecting_wall(segment_a, segment_b).unwrap();
    /*carve_line(
        world,
        Pos::new(connecting_wall.x1, connecting_wall.y1),
        Pos::new(connecting_wall.x2, connecting_wall.y2),
        1,
        TileKind::BlackFloor,
    );*/

    // Wall must be 1-thick
    assert!(connecting_wall.x1 == connecting_wall.x2 || connecting_wall.y1 == connecting_wall.y2);

    // (is connecting wall vertical/horizontal, what coord is it)
    let (vertical, dividing_dim_val) = if connecting_wall.x1 == connecting_wall.x2 {
        (true, connecting_wall.x1)
    } else {
        (false, connecting_wall.y1)
    };

    // distance from door to wall
    let dist_to_wall = |p: Pos| {
        if vertical {
            (p.x - dividing_dim_val).abs()
        } else {
            (p.y - dividing_dim_val).abs()
        }
    };

    let get_ideal_doors = |all_doors: &Vec<_>| {
        // min of of (dist to closest wall)
        // is there always such a door?
        let min_dist = all_doors
            .iter()
            .map(|&a_pos| dist_to_wall(a_pos))
            .min()
            .unwrap();
        let doors_to_connect: Vec<_> = a_doors
            .iter()
            .filter(|&door| dist_to_wall(*door) == min_dist)
            .collect();

        doors_to_connect
    };

    // so here's the doors that are likely on the same corridor
    let a_ideal_doors = get_ideal_doors(a_doors);
    let b_ideal_doors = get_ideal_doors(b_doors);

    let mut all_ideal_doors: Vec<Pos> = Vec::new();
    all_ideal_doors.extend(a_ideal_doors);
    all_ideal_doors.extend(b_ideal_doors);

    // now we carve little passages from the door to the corridors
    for ideal_door in all_ideal_doors.iter() {
        let inner_corridor_range = if vertical {
            ideal_door.x..dividing_dim_val
        } else {
            ideal_door.y..dividing_dim_val
        };
        for v in inner_corridor_range {
            let p = if vertical {
                Pos::new(v, ideal_door.y)
            } else {
                Pos::new(ideal_door.x, v)
            };

            world[p].kind = TileKind::BlackFloor;
        }
    }

    // Now let's get the endpoints of the corridor
    // TODO: need to clean this up
    let corridor_start = if vertical {
        Pos::new(
            dividing_dim_val,
            all_ideal_doors.iter().map(|d| d.y).min().unwrap(),
        )
    } else {
        Pos::new(
            all_ideal_doors.iter().map(|d| d.x).min().unwrap(),
            dividing_dim_val,
        )
    };

    let corridor_end = if vertical {
        Pos::new(
            dividing_dim_val,
            all_ideal_doors.iter().map(|d| d.y).max().unwrap(),
        )
    } else {
        Pos::new(
            all_ideal_doors.iter().map(|d| d.x).max().unwrap(),
            dividing_dim_val,
        )
    };

    // Carve out the corridor
    // TODO: again I feel like this can be written a lot more simply
    if vertical {
        for v in corridor_start.y..corridor_end.y + 1 {
            let p = Pos::new(dividing_dim_val, v);
            world[p].kind = TileKind::BlackFloor;
        }
    } else {
        for v in corridor_start.x..corridor_end.x + 1 {
            let p = Pos::new(v, dividing_dim_val);
            world[p].kind = TileKind::BlackFloor;
        }
    }
}
