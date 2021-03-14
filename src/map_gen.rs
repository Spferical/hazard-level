use rand::rngs::SmallRng;
use rand::Rng;
use rand::{seq::SliceRandom, SeedableRng};

use crate::world::{Item, Mob, MobKind, Offset, Pos, Rect, TileKind, World, DIRECTIONS};

macro_rules! avg {
    ($n: expr, $d: expr) => {
        ($n + $d) / 2
    };
}

pub struct CarveRoomOpts {
    wall: TileKind,
    floor: TileKind,
    max_width: i32,
    max_height: i32,
    min_width: i32,
    min_height: i32,
}

pub fn carve_rooms_bsp_extra_loops(
    world: &mut World,
    rect: Rect,
    opts: &CarveRoomOpts,
    rng: &mut impl Rng,
    loopiness: f32,
) -> Vec<Rect> {
    let rooms = carve_rooms_bsp(world, rect, opts, rng);
    for _ in 0..(rooms.len() as f32 * loopiness) as u32 {
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

pub fn carve_rooms_bsp(
    world: &mut World,
    rect: Rect,
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
            let mut rooms = carve_rooms_bsp(
                world,
                Rect::new(rect.x1, split_x - 1, rect.y1, rect.y2),
                opts,
                rng,
            );
            rooms.extend(carve_rooms_bsp(
                world,
                Rect::new(split_x + 1, rect.x2, rect.y1, rect.y2),
                opts,
                rng,
            ));
            let mid_left = Pos {
                x: avg!(rect.x1, split_x - 1),
                y: avg!(rect.y1, rect.y2),
            };
            let mid_right = Pos {
                x: avg!(split_x + 1, rect.x2),
                y: avg!(rect.y1, rect.y2),
            };
            carve_line(world, mid_left, mid_right, 0, opts.floor);
            rooms
        }
        Split::Y => {
            let split_y = rng.gen_range(rect.y1 + opts.min_height + 1..(rect.y2 - opts.min_height));
            let mut rooms = carve_rooms_bsp(
                world,
                Rect::new(rect.x1, rect.x2, rect.y1, split_y - 1),
                opts,
                rng,
            );
            rooms.extend(carve_rooms_bsp(
                world,
                Rect::new(rect.x1, rect.x2, split_y + 1, rect.y2),
                opts,
                rng,
            ));
            let mid_top = Pos {
                x: avg!(rect.x1, rect.x2),
                y: avg!(rect.y1, split_y - 1),
            };
            let mid_bot = Pos {
                x: avg!(rect.x1, rect.x2),
                y: avg!(split_y + 1, rect.y2),
            };
            carve_line(world, mid_top, mid_bot, 0, opts.floor);
            rooms
        }
        Split::None => {
            // just carve the room
            fill_rect(
                world,
                Rect::new(rect.x1, rect.x2, rect.y1, rect.y2),
                opts.floor,
            );
            vec![rect]
        }
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

fn gen_offices(
    world: &mut World,
    rng: &mut impl Rng,
    left_entrance: Pos,
    rect: Rect,
) -> (Pos, Offset) {
    // offices
    fill_rect(world, rect, TileKind::Wall);
    carve_floor(world, left_entrance, 1, TileKind::Floor);
    let bsp_opts = CarveRoomOpts {
        wall: TileKind::Wall,
        floor: TileKind::Floor,
        max_width: 10,
        max_height: 10,
        min_width: 2,
        min_height: 2,
    };
    let rect = Rect::new(rect.x1 + 1, rect.x2 - 1, rect.y1 + 1, rect.y2 - 1);
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
    // spawn some enemies
    for _ in 0..100 {
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

    let room = rooms.choose(rng).unwrap();
    let pos = room.choose(rng);
    world.mobs.insert(pos, Mob::new(MobKind::Sculpture));

    // spawn some ammo
    for _ in 0..20 {
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

    let rightmost_room = **rooms
        .iter()
        .filter(|r| r.x2 == rect.x2)
        .collect::<Vec<_>>()
        .choose(rng)
        .unwrap();
    let center_y = avg!(rightmost_room.y1, rightmost_room.y2);
    let right_wall = Pos {
        x: rect.x2 + 1,
        y: center_y,
    };
    carve_floor(world, right_wall, 0, TileKind::Floor);
    (right_wall, Offset { x: 1, y: 0 })
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
    fill_rect(world, Rect::new(-50, 40, -50, 50), TileKind::Ocean);
    fill_rect(world, Rect::new(-10, 10, -10, 10), TileKind::BlackFloor);
    // h for helicopter
    fill_rect(world, Rect::new(-3, -3, -3, 3), TileKind::YellowFloor);
    fill_rect(world, Rect::new(3, 3, -3, 3), TileKind::YellowFloor);
    fill_rect(world, Rect::new(-3, 3, 0, 0), TileKind::YellowFloor);

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
