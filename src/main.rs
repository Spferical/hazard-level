use crate::world::Item;
use crate::world::MissionState;
use crate::world::MobKind;
use crate::world::PLAYER_MAX_HEALTH;
use crate::world::{Offset, Pos, TileKind};
use bracket_lib::prelude::*;
use rand::rngs::SmallRng;
use rand::SeedableRng;
use std::collections::HashSet;

mod fov;
mod map_gen;
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
const LIGHT_PURPLE: NamedColor = (0xf8, 0x54, 0xf8);

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
    rng: SmallRng,
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
        (TileKind::Fire, true) => ("!", DARK_RED, LIGHT_RED),
        (TileKind::Fire, false) => ("!", DARK_RED, DARK_BLACK),
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
        (MobKind::OldMan, _) => ("@", LIGHT_PURPLE),
        (MobKind::Alien, _) => (",", LIGHT_YELLOW),
    };
    MobPrintable {
        symbol,
        fg: RGB::named(fg),
    }
}

fn get_item_printable(item: Item, visible: bool) -> MobPrintable {
    let (symbol, fg) = match (item, visible) {
        (Item::Corpse, _) => ("%", DARK_RED),
        (Item::Ammo, _) => ("=", LIGHT_BLUE),
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
        let moved = self.gs.move_player(off);
        if let Some(item) = self.gs.pick_up_item() {
            match item {
                Item::Corpse => panic!(),
                Item::Ammo => {
                    let eff = Effect::Text {
                        color: RGB::named(LIGHT_BLUE),
                        pos: Pos::new(35, 23),
                        text: "more ammo!".to_string(),
                        time_left: 1.0,
                    };
                    self.effects.push(eff);
                }
            }
        }
        moved
    }

    fn fire(&mut self, off: world::Offset) -> bool {
        let player_pos = self.gs.world.player_pos();
        if let Some(end_pos) = self.gs.world.fire(player_pos, off) {
            let eff = Effect::Line {
                color: RGB::named(LIGHT_WHITE),
                p1: player_pos,
                p2: end_pos,
                time_total: 0.2,
                time_left: 0.2,
            };
            self.effects.push(eff);
        } else {
            let eff = Effect::Text {
                color: RGB::named(LIGHT_YELLOW),
                pos: Pos::new(35, 23),
                text: "click".to_string(),
                time_left: 1.0,
            };
            self.effects.push(eff);
        }
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

    fn map_to_screen(&self, pos: Pos, map_rect: Rect) -> Pos {
        self.map_rect_to_screen(self.map_to_map_rect(pos, map_rect), map_rect)
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
                let visible = seen.contains(&map_pos);
                let mut printable = get_printable(world[map_pos].kind, visible);
                if world[map_pos].blood {
                    printable.fg = RGB::named(DARK_RED);
                }
                if world[map_pos].rust {
                    printable.fg = RGB::named(DARK_YELLOW);
                }
                if let Some(item) = world[map_pos].item {
                    let MobPrintable { fg, symbol } = get_item_printable(item, visible);
                    printable.fg = fg;
                    printable.symbol = symbol;
                }
                if let Some(mob) = self.gs.player_memory.mobs.get(&map_pos) {
                    let MobPrintable { fg, symbol } = get_mob_printable(mob.kind, visible);
                    printable.fg = fg;
                    printable.symbol = symbol;
                }
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

    fn print_multi(&mut self, ctx: &mut BTerm, mut x: i32, y: i32, texts: &[(String, RGB)]) {
        for (text, color) in texts {
            ctx.print_color(x, y, *color, RGB::named(DARK_BLACK), text);
            x += text.len() as i32;
        }
    }

    fn get_map_rect(ctx: &BTerm) -> Rect {
        let (w, h) = ctx.get_char_size();
        let map_rect = Rect {
            x: 0,
            y: 0,
            w: w as i32,
            h: h as i32 - 2,
        };
        map_rect
    }

    fn draw(&mut self, ctx: &mut BTerm) {
        let (_w, h) = ctx.get_char_size();
        let map_rect = Self::get_map_rect(ctx);
        let player_pos = self.gs.world.player_pos();
        let seen = fov::calculate_fov(player_pos, FOV_RANGE, &self.gs.world);
        self.draw_map(ctx, map_rect, &seen);
        self.handle_effects(ctx, map_rect, &seen);

        let mut statusbar = Vec::new();
        statusbar.push(("ammo:".to_string(), RGB::named(LIGHT_WHITE)));
        let color = match self.gs.world.player_ammo {
            0 => RGB::named(DARK_RED),
            1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 => RGB::named(LIGHT_RED),
            _ => RGB::named(LIGHT_GREEN),
        };
        statusbar.push((self.gs.world.player_ammo.to_string(), color));
        statusbar.push((" hp:".to_string(), RGB::named(LIGHT_WHITE)));
        let color = match self.gs.world.player_damage() {
            0 => RGB::named(LIGHT_GREEN),
            1 | 2 | 3 => RGB::named(LIGHT_YELLOW),
            _ => RGB::named(LIGHT_RED),
        };
        statusbar.push((
            (PLAYER_MAX_HEALTH - self.gs.world.player_damage()).to_string() + " ",
            color,
        ));
        statusbar.push(match self.gs.state {
            MissionState::Start => (
                "Find the computer and enter the code...".to_string(),
                RGB::named(LIGHT_BLUE),
            ),
            MissionState::CodeEntered { seconds_left: _ } => (
                "CODE ENTERED! Now get outta here!".to_string(),
                RGB::named(LIGHT_RED),
            ),
            MissionState::Win => ("YOU WIN!".to_string(), RGB::named(DARK_GREEN)),
        });
        self.print_multi(ctx, 0, h as i32 - 2, &statusbar);

        ctx.print_color_centered(
            h as i32 - 1,
            RGB::named(LIGHT_WHITE),
            RGB::named(DARK_BLACK),
            "move:←↓↑→ shoot:shift+move wait:space",
        );
    }

    fn random_text_effect(self: &mut Self, ctx: &mut BTerm, text: String, time: f32, color: RGB) {
        let (w, h) = ctx.get_char_size();
        let rect = world::Rect {
            x1: 0,
            y1: 0,
            x2: w as i32 - text.len() as i32 - 1,
            y2: h as i32 - 1,
        };
        let pos = rect.choose(&mut self.rng);
        self.effects.push(Effect::Text {
            color,
            pos,
            text,
            time_left: time,
        });
    }
}

fn player_input(ui: &mut Ui, ctx: &mut BTerm) {
    use VirtualKeyCode::*;
    let dt = ctx.frame_time_ms;
    if ctx.key.is_some() && ui.gs.player_is_dead() {
        ui.random_text_effect(ctx, "DEAD".to_string(), 1f32, RGB::named(DARK_RED));
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
    if moved {
        if let MissionState::CodeEntered { seconds_left } = ui.gs.state {
            if seconds_left % 10 == 0 {
                ui.random_text_effect(
                    ctx,
                    format!("self destruct in T minus {} seconds", seconds_left),
                    1.0,
                    RGB::named(DARK_RED),
                );
            }
        }
    }
    for (pos, effect) in ui.gs.tick(dt, moved, &mut ui.rng) {
        let pos = ui.map_to_screen(pos, Ui::get_map_rect(ctx));
        match effect {
            world::Effect::Creak => {
                ui.effects.push(Effect::Text {
                    color: RGB::named(DARK_RED),
                    pos,
                    text: "*creak*".to_string(),
                    time_left: 1.0,
                });
            }
            world::Effect::Shuffle => {
                ui.effects.push(Effect::Text {
                    color: RGB::named(DARK_YELLOW),
                    pos,
                    text: "*shuffle*".to_string(),
                    time_left: 1.0,
                });
            }
            world::Effect::Hiss => {
                ui.effects.push(Effect::Text {
                    color: RGB::named(DARK_GREEN),
                    pos,
                    text: "*hiss*".to_string(),
                    time_left: 1.0,
                });
            }
        }
    }
}

fn main() {
    let context = BTermBuilder::simple80x50()
        .with_title("Run")
        .with_fps_cap(30.0)
        .build()
        .unwrap();
    let mut gs = world::GameState::new();
    gs.generate_world(RandomNumberGenerator::new().rand());
    let ui = Ui {
        gs,
        effects: vec![],
        rng: SmallRng::seed_from_u64(72),
    };
    main_loop(context, ui).unwrap();
}
