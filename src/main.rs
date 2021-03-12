use crate::world::MobKind;
use crate::world::PLAYER_MAX_HEALTH;
use crate::world::{Offset, Pos, TileKind};
use bracket_lib::prelude::*;
use std::collections::HashSet;

mod fov;
mod world;

const FOV_RANGE: i32 = 8;

type NamedColor = (u8, u8, u8);

const DARK_BLACK: NamedColor = (0x1c, 0x1c, 0x1c);
const DARK_WHITE: NamedColor = (0x9c, 0x99, 0x8e);
const LIGHT_BLACK: NamedColor = (0x54, 0x50, 0x54);
const LIGHT_WHITE: NamedColor = (0xf8, 0xfc, 0xf8);
const DARK_YELLOW: NamedColor = (0xff, 0xad, 0x33);
const DARK_RED: NamedColor = (0xff, 0x33, 0x33);
const LIGHT_RED: NamedColor = (0xe5, 0x78, 0x6d);
const DARK_BLUE: NamedColor = (0x33, 0x63, 0xa1);
const LIGHT_BLUE: NamedColor = (0x77, 0xa7, 0xe5);
const LIGHT_YELLOW: NamedColor = (0xf8, 0xfc, 0x50);

impl From<world::Pos> for Point {
    fn from(pos: world::Pos) -> Self {
        let Pos { x, y } = pos;
        Self { x, y }
    }
}

impl From<Point> for world::Pos {
    fn from(pt: Point) -> world::Pos {
        let Point { x, y } = pt;
        Self { x, y }
    }
}

enum Effect {
    Line {
        p1: Pos,
        p2: Pos,
        color: RGB,
        time_total: f32,
        time_left: f32,
    },
    Text {
        pos: Pos,
        text: String,
        color: RGB,
        time_left: f32,
    },
}

struct Ui {
    gs: world::GameState,
    effects: Vec<Effect>,
}

impl GameState for Ui {
    fn tick(&mut self, ctx: &mut BTerm) {
        player_input(self, ctx);
        ctx.cls();
        self.draw(ctx);
    }
}

struct TilePrintable {
    symbol: &'static str,
    fg: RGB,
    bg: RGB,
}

fn get_printable(tile_kind: TileKind, visible: bool) -> TilePrintable {
    let (symbol, fg, bg) = match (tile_kind, visible) {
        (TileKind::Wall, true) => ("#", DARK_BLACK, LIGHT_WHITE),
        (TileKind::Wall, false) => ("#", LIGHT_BLACK, DARK_WHITE),
        (TileKind::Floor, true) => (".", LIGHT_WHITE, LIGHT_BLACK),
        (TileKind::Floor, false) => (".", DARK_WHITE, DARK_BLACK),
        (TileKind::Ocean, false) => ("~", DARK_BLUE, DARK_BLACK),
        (TileKind::Ocean, true) => ("~", LIGHT_BLUE, DARK_BLACK),
        (TileKind::BlackFloor, false) => (".", LIGHT_BLACK, DARK_BLACK),
        (TileKind::BlackFloor, true) => (".", DARK_BLACK, LIGHT_BLACK),
        (TileKind::YellowFloor, true) => (".", LIGHT_YELLOW, LIGHT_BLACK),
        (TileKind::YellowFloor, false) => (".", LIGHT_YELLOW, DARK_BLACK),
        (TileKind::BloodyFloor, false) => (".", DARK_RED, DARK_BLACK),
        (TileKind::BloodyFloor, true) => (".", DARK_RED, LIGHT_BLACK),
        (TileKind::Unseen, _) => (" ", DARK_BLACK, DARK_BLACK),
        (_, _) => ("?", LIGHT_BLUE, DARK_BLACK),
    };
    TilePrintable {
        symbol,
        fg: RGB::named(fg),
        bg: RGB::named(bg),
    }
}

struct MobPrintable {
    symbol: &'static str,
    fg: RGB,
}

fn get_mob_printable(kind: MobKind, visible: bool) -> MobPrintable {
    let (symbol, fg) = match (kind, visible) {
        (MobKind::Zombie, _) => ("@", DARK_YELLOW),
    };
    MobPrintable {
        symbol,
        fg: RGB::named(fg),
    }
}

#[derive(Clone, Copy)]
struct Rect {
    x: i32,
    y: i32,
    w: i32,
    h: i32,
}

impl Ui {
    fn move_player(&mut self, off: world::Offset) -> bool {
        self.gs.move_player(off)
    }

    fn fire(&mut self, off: world::Offset) -> bool {
        let player_pos = self.gs.world.player_pos();
        let end_pos = self.gs.world.fire(player_pos, off);
        let eff = Effect::Line {
            color: RGB::named(LIGHT_WHITE),
            p1: player_pos,
            p2: end_pos,
            time_total: 0.2,
            time_left: 0.2,
        };
        self.effects.push(eff);
        true
    }

    fn handle_effects(&mut self, ctx: &mut BTerm, map_rect: Rect, seen: &HashSet<Pos>) {
        let effects: Vec<_> = self.effects.drain(..).collect();
        self.effects = effects
            .into_iter()
            .filter_map(|eff| match eff {
                Effect::Line {
                    p1,
                    p2,
                    color,
                    time_total,
                    mut time_left,
                } => {
                    for pt in line2d(LineAlg::Bresenham, p1.into(), p2.into()) {
                        let tile = self.gs.world[pt.into()];
                        let map_bg = get_printable(tile.kind, seen.contains(&pt.into())).bg;
                        let rect_pos = self.map_to_map_rect(pt.into(), map_rect);
                        let screen_pos = self.map_rect_to_screen(rect_pos, map_rect);
                        let percent_done = (time_total - time_left) / time_total;
                        let color = color.lerp(map_bg, percent_done);
                        ctx.set_bg(screen_pos.x, screen_pos.y, color);
                    }
                    time_left -= ctx.frame_time_ms / 1000f32;
                    if time_left > 0f32 {
                        Some(Effect::Line {
                            p1,
                            p2,
                            color,
                            time_left,
                            time_total,
                        })
                    } else {
                        None
                    }
                }
                Effect::Text {
                    pos,
                    text,
                    color,
                    mut time_left,
                } => {
                    ctx.print_color(pos.x, pos.y, color, RGBA::from_u8(0, 0, 0, 0), &text);
                    time_left -= ctx.frame_time_ms / 1000f32;
                    if time_left > 0f32 {
                        Some(Effect::Text {
                            pos,
                            text,
                            color,
                            time_left,
                        })
                    } else {
                        None
                    }
                }
            })
            .collect();
    }

    fn map_rect_to_screen(&self, pos: Pos, map_rect: Rect) -> Pos {
        pos + Offset {
            x: map_rect.x,
            y: map_rect.y,
        }
    }

    fn map_to_map_rect(&self, pos: Pos, map_rect: Rect) -> Pos {
        let player_pos = self.gs.world.player_pos();
        pos + Offset {
            x: -player_pos.x + map_rect.w / 2,
            y: -player_pos.y + map_rect.h / 2,
        }
    }

    fn map_rect_to_map(&self, pos: Pos, map_rect: Rect) -> Pos {
        let player_pos = self.gs.world.player_pos();
        pos + Offset {
            x: player_pos.x - map_rect.w / 2,
            y: player_pos.y - map_rect.h / 2,
        }
    }

    fn draw_map(&mut self, ctx: &mut BTerm, screen_rect: Rect, seen: &HashSet<Pos>) {
        let gs = &self.gs;
        for rect_x in 0..screen_rect.w {
            for rect_y in 0..screen_rect.h {
                let rect_pos = Pos {
                    x: rect_x,
                    y: rect_y,
                };
                let map_pos = self.map_rect_to_map(rect_pos, screen_rect);
                let world = if gs.debug_mode {
                    &gs.world
                } else {
                    &gs.player_memory
                };
                let printable = get_printable(world[map_pos].kind, seen.contains(&map_pos));
                let screen_pos = self.map_rect_to_screen(rect_pos, screen_rect);

                ctx.print_color(
                    screen_pos.x,
                    screen_pos.y,
                    printable.fg,
                    printable.bg,
                    printable.symbol,
                );

                if let Some(mob) = self.gs.player_memory.mobs.get(&map_pos) {
                    let mob_printable = get_mob_printable(mob.kind, seen.contains(&map_pos));
                    ctx.print_color(
                        screen_pos.x,
                        screen_pos.y,
                        mob_printable.fg,
                        printable.bg,
                        mob_printable.symbol,
                    );
                }
            }
        }
        let player_pos = self.gs.world.player_pos();
        let bg = get_printable(gs.world[player_pos].kind, true).bg;
        let bg = bg.lerp(
            RGB::named(DARK_RED),
            gs.world.player_damage() as f32 / PLAYER_MAX_HEALTH as f32,
        );

        ctx.print_color(
            screen_rect.x + screen_rect.w / 2,
            screen_rect.y + screen_rect.h / 2,
            RGB::named(LIGHT_WHITE),
            bg,
            "@",
        );
    }

    fn draw(&mut self, ctx: &mut BTerm) {
        let (w, h) = ctx.get_char_size();
        let map_rect = Rect {
            x: 0,
            y: 0,
            w: w as i32,
            h: h as i32 - 1,
        };
        let player_pos = self.gs.world.player_pos();
        let seen = fov::calculate_fov(player_pos, FOV_RANGE, &self.gs.world);
        self.draw_map(ctx, map_rect, &seen);
        self.handle_effects(ctx, map_rect, &seen);
        ctx.print_color_centered(
            h as i32 - 1,
            RGB::named(LIGHT_WHITE),
            RGB::named(DARK_BLACK),
            "move:←↓↑→ shoot:shift+move wait:space",
        );
        ctx.print_color_centered(
            0,
            RGB::named(LIGHT_WHITE),
            RGB::named(DARK_BLACK),
            &format!("{}", ctx.shift),
        );
    }
}

fn player_input(ui: &mut Ui, ctx: &mut BTerm) {
    use VirtualKeyCode::*;
    let dt = ctx.frame_time_ms;
    if ctx.key.is_some() && ui.gs.world.player_is_dead() {
        ui.effects.push(Effect::Text {
            pos: Pos { x: 20, y: 20 },
            color: RGB::named(DARK_RED),
            text: "DEAD".to_string(),
            time_left: 1f32,
        });
        return;
    }
    // Player movement
    let moved = match ctx.key {
        None => false, // Nothing happened
        Some(key) => {
            if !ctx.shift {
                match key {
                    Left | H => ui.move_player(world::Offset { x: -1, y: 0 }),
                    Right | L => ui.move_player(world::Offset { x: 1, y: 0 }),
                    Up | K => ui.move_player(world::Offset { x: 0, y: -1 }),
                    Down | J => ui.move_player(world::Offset { x: 0, y: 1 }),
                    Space => true,
                    Tab => {
                        ui.gs.debug_mode = !ui.gs.debug_mode;
                        false
                    }
                    _ => false,
                }
            } else {
                match key {
                    Left | H => ui.fire(world::Offset { x: -1, y: 0 }),
                    Right | L => ui.fire(world::Offset { x: 1, y: 0 }),
                    Up | K => ui.fire(world::Offset { x: 0, y: -1 }),
                    Down | J => ui.fire(world::Offset { x: 0, y: 1 }),
                    _ => false,
                }
            }
        }
    };
    ui.gs.tick(dt, moved);
}

fn main() {
    let context = BTermBuilder::simple80x50()
        .with_title("Roguelike Tutorial")
        .build()
        .unwrap();
    let mut gs = world::GameState::new();
    gs.generate_world(RandomNumberGenerator::new().rand());
    let ui = Ui {
        gs,
        effects: vec![],
    };
    main_loop(context, ui).unwrap();
}
