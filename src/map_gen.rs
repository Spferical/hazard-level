use std::collections::HashMap;
use std::io::Cursor;

use rand::rngs::SmallRng;
use rand::Rng;
use rand::{seq::SliceRandom, SeedableRng};

use image;

use crate::world::{Item, Mob, MobKind, Offset, Pos, Rect, TileKind, World, DIRECTIONS};

pub const SEGMENTS: &[&[u8]] = &[
    include_bytes!("../static/0.ppm"),
    include_bytes!("../static/0.ppm"), // make more stuff
    include_bytes!("../static/0.ppm"),
    include_bytes!("../static/0.ppm"),
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
    assert!(opts.min_width * 2 + 1 < opts.max_width);
    assert!(opts.min_height * 2 + 1 < opts.max_height);
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
) {
    let mut pos = start;
    while pos != end {
        let dir = if rng.gen::<f64>() < waviness {
            *DIRECTIONS.choose(rng).unwrap()
        } else {
            (end - pos).closest_dir()
        };
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

fn gen_offices(world: &mut World, rng: &mut impl Rng, entrances: &[Pos], rect: Rect) {
    // offices
    for entrance in entrances {
        carve_floor(world, *entrance, 1, TileKind::Floor);
    }
    let bsp_opts = CarveRoomOpts {
        wall: TileKind::Wall,
        floor: TileKind::Floor,
        max_width: 6,
        max_height: 6,
        min_width: 2,
        min_height: 2,
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
    for _ in 0..rng.gen_range(0..=(size / 20).max(1)) {
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
    for _ in 0..rng.gen_range(0..size / 50).max(1) {
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
        max_width: 30,
        max_height: 30,
        min_width: 10,
        min_height: 10,
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

    // finally, gen the intermediate rooms
    for room in room_graph.iter() {
        let adjs = room_graph.get_adj(room).unwrap();
        // carve_room(world, room, adjs, &mut rng, TileKind::Floor);
        let entrances = adjs
            .into_iter()
            .map(|adj| get_connecting_wall(room, *adj).unwrap())
            .map(|r| r.choose(&mut rng))
            .collect::<Vec<_>>();
        gen_offices(world, &mut rng, &entrances, room);
        for entrance in entrances {
            carve_floor(world, entrance, 0, TileKind::Floor);
        }
    }

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

    /*
        let (mut edge, dir) = gen_offices(world, &mut rng, Pos::new(8, 0), Rect::new(8, 50, -25, 25));
        carve_floor(world, edge, 0, TileKind::Floor);
        edge += dir;
        let (mut edge, dir) = generate_containment(world, &mut rng, edge);
        edge += dir;
        carve_floor(world, edge, 0, TileKind::Floor);

        // goal
        fill_rect(
            world,
            Rect::new(edge.x + 1, edge.x + 4, edge.y - 2, edge.y + 2),
            TileKind::BloodyFloor,
        );
        carve_floor(world, edge + Offset { x: 2, y: 0 }, 0, TileKind::Computer);
        //gen_lab_complex(world, &mut rng, Pos::new(8, 0), Rect::new(8, 50, -25, 25));
    */
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

pub fn gen_lab_complex(world: &mut World, rng: &mut impl Rng, left_entrance: Pos, rect: Rect) {
    fill_rect(world, rect, TileKind::Wall);
    carve_floor(world, left_entrance, 1, TileKind::Floor);
    let bsp_opts = BspSplitOpts {
        max_width: 100,
        max_height: 100,
        min_width: 20,
        min_height: 20,
    };

    let tree = gen_bsp_tree(rect, bsp_opts, rng);
    let room_graph = tree.into_room_graph();

    // todo: add non-tree connections
    // maybe add them as optional connections for the segment carver

    /*
    for (room, adj_rooms) in rooms_with_adj {
        carve_segment(world, 0, room, TileKind::Floor, rng);
    }
    */
}

pub fn carve_segment(
    world: &mut World,
    segment_id: usize,
    segment_dimensions: Rect,
    floor_type: TileKind,
    rng: &mut impl Rng,
) {
    let img_bytes = SEGMENTS[segment_id];
    let img = image::io::Reader::new(Cursor::new(img_bytes))
        .with_guessed_format()
        .expect("unable to read ppm file in memory??")
        .decode()
        .unwrap();

    let room_width = (segment_dimensions.x2 - segment_dimensions.x1) as u32 - 2;
    let room_height = (segment_dimensions.y2 - segment_dimensions.y1) as u32 - 2;

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
        image::imageops::FilterType::Lanczos3,
    );

    let mut nexuses: Vec<Pos> = Vec::new();

    // Carve out rooms
    for (x, y, pixel) in img.enumerate_pixels() {
        let alpha = pixel[0];
        let nexus = pixel[1];

        let pos = Pos::new(x as i32, y as i32);

        if nexus > 0 {
            nexuses.push(pos);
        } else {
            // alpha is 0 if it is a wall, 255 if it is empty
            if rng.gen_range(0..255) < alpha {
                // we carve it out
                world[pos].kind = floor_type;
            }
        }
    }
}
