use bracket_lib::prelude::*;
use std::collections::HashSet;

mod fov;
mod world;

use crate::world::{Offset, Pos, TileKind};

const FOV_RANGE: i32 = 8;

type NamedColor = (u8, u8, u8);

const DARK_BLACK: NamedColor = (0x1c, 0x1c, 0x1c);
const DARK_WHITE: NamedColor = (0x9c, 0x99, 0x8e);
const LIGHT_BLACK: NamedColor = (0x54, 0x50, 0x54);
const LIGHT_WHITE: NamedColor = (0xf8, 0xfc, 0xf8);

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
        (TileKind::Unseen, _) => (" ", DARK_BLACK, DARK_BLACK),
    };
    TilePrintable {
        symbol,
        fg: RGB::named(fg),
        bg: RGB::named(bg),
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
    fn move_player(&mut self, off: world::Offset) {
        self.gs.move_player(off);
    }

    fn fire(&mut self, off: world::Offset) {
        let player = self.gs.world.get_player();
        let end_pos = self.gs.world.fire(player.pos, off);
        let eff = Effect::Line {
            color: RGB::named(LIGHT_WHITE),
            p1: player.pos,
            p2: end_pos,
            time_total: 0.2,
            time_left: 0.2,
        };
        self.effects.push(eff);
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
        let player = self.gs.world.get_player();
        pos + Offset {
            x: -player.pos.x + map_rect.w / 2,
            y: -player.pos.y + map_rect.h / 2,
        }
    }

    fn map_rect_to_map(&self, pos: Pos, map_rect: Rect) -> Pos {
        let player = self.gs.world.get_player();
        pos + Offset {
            x: player.pos.x - map_rect.w / 2,
            y: player.pos.y - map_rect.h / 2,
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
            }
        }
        let player = self.gs.world.get_player();
        ctx.print_color(
            screen_rect.x + screen_rect.w / 2,
            screen_rect.y + screen_rect.h / 2,
            RGB::named(LIGHT_WHITE),
            get_printable(gs.world[player.pos].kind, true).bg,
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
        let player = self.gs.world.get_player();
        let seen = fov::calculate_fov(player.pos, FOV_RANGE, &self.gs.world);
        self.draw_map(ctx, map_rect, &seen);
        self.handle_effects(ctx, map_rect, &seen);
        ctx.print_color_centered(
            h as i32 - 1,
            RGB::named(LIGHT_WHITE),
            RGB::named(DARK_BLACK),
            "move:←↓↑→ shoot:shift+move",
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
    // Player movement
    match ctx.key {
        None => {} // Nothing happened
        Some(key) => {
            if !ctx.shift {
                match key {
                    Left | H => ui.move_player(world::Offset { x: -1, y: 0 }),
                    Right | L => ui.move_player(world::Offset { x: 1, y: 0 }),
                    Up | K => ui.move_player(world::Offset { x: 0, y: -1 }),
                    Down | J => ui.move_player(world::Offset { x: 0, y: 1 }),
                    _ => {}
                }
            } else {
                match key {
                    Left | H => ui.fire(world::Offset { x: -1, y: 0 }),
                    Right | L => ui.fire(world::Offset { x: 1, y: 0 }),
                    Up | K => ui.fire(world::Offset { x: 0, y: -1 }),
                    Down | J => ui.fire(world::Offset { x: 0, y: 1 }),
                    _ => {}
                }
            }
        }
    }
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
