use crate::world::Item;
use crate::world::MissionState;
use crate::world::MobKind;
use crate::world::PLAYER_MAX_HEALTH;
use crate::world::{Offset, Pos, TileKind};
use bracket_lib::prelude::*;
use chrono::prelude::*;
use rand::rngs::SmallRng;
use rand::seq::SliceRandom;
use rand::Rng;
use rand::SeedableRng;
use std::collections::HashSet;
use textwrap;

mod fov;
#[macro_use]
mod map_gen;
mod world;

const FOV_RANGE: i32 = 8;
const DESCRIPTION_WIDTH: i32 = 15;
const MOB_DESCRIPTION_HEIGHT: i32 = 5;
const SENSING_HEIGHT: i32 = 30;

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
const DARK_PURPLE: NamedColor = (0xa8, 0x00, 0xa8);

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

impl Ui {
    fn add_text_effect_1s<T: ToString>(&mut self, mut pos: Pos, text: T, color: RGB) {
        // try not to overlap with the exact position
        pos.x += 1;
        self.effects.push(Effect::Text {
            pos,
            text: text.to_string(),
            color,
            time_left: 1.0,
        })
    }
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
        (TileKind::YellowFloor, true) => (".", DARK_YELLOW, LIGHT_BLACK),
        (TileKind::YellowFloor, false) => (".", DARK_YELLOW, DARK_BLACK),
        (TileKind::YellowWall, true) => ("#", LIGHT_YELLOW, DARK_BLACK),
        (TileKind::YellowWall, false) => ("#", DARK_YELLOW, DARK_BLACK),
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
        (MobKind::Sculpture, _) => ("&", DARK_YELLOW),
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
        // thing effects
        let thing_pos = gs.world.thing.pos;
        let thing_dist = (thing_pos - self.gs.world.player_pos()).mhn_dist().max(1);
        let thing_noise_strength = (1.0 / thing_dist as f32).min(1.0 / 5.0);
        let noise_speed = thing_noise_strength * 100.0;
        let mut thing_noise = FastNoise::seeded((gs.world.thing.elapsed * noise_speed) as u64);
        thing_noise.set_frequency(1.0 / thing_dist as f32);

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
                    printable.bg = RGB::named(DARK_PURPLE);
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

                let noise_factor = thing_noise_strength
                    * thing_noise.get_noise(map_pos.x as f32, map_pos.y as f32);

                ctx.print_color(
                    screen_pos.x,
                    screen_pos.y,
                    printable.fg.lerp(RGB::named(BLACK), noise_factor),
                    printable.bg.lerp(RGB::named(BLACK), noise_factor),
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

        if seen.contains(&self.gs.world.thing.pos) || self.gs.debug_mode {
            let bg = RGB::named(DARK_BLACK);
            let fg = RGB::named(LIGHT_WHITE);
            let screen_pos = self.map_to_screen(self.gs.world.thing.pos, screen_rect);
            let thing_chars: Vec<char> =
                "QWERTYUIOPASDFGHJKLZXCVBNM!@#$%^&*():;~".chars().collect();
            let thing_char = thing_chars.as_slice().choose(&mut self.rng).unwrap();
            ctx.print_color(screen_pos.x, screen_pos.y, fg, bg, thing_char);
        }
        let thing_fov = fov::calculate_fov(thing_pos, FOV_RANGE, &self.gs.world);
        for pos in thing_fov {
            if seen.contains(&pos) && self.gs.world[pos].kind.is_walkable() {
                if self.rng.gen::<f32>() < 0.1 {
                    let screen_pos = self.map_to_screen(pos, screen_rect);
                    ctx.print_color(
                        screen_pos.x,
                        screen_pos.y,
                        RGB::named(BLACK),
                        RGB::named(BLACK),
                        " ",
                    );
                }
            }
        }
    }

    fn print_rect(
        &mut self,
        ctx: &mut BTerm,
        rect: Rect,
        texts: &[String],
        color: RGB,
        y_off: i32,
    ) {
        let mut y = rect.y + y_off;
        for text in texts {
            if y >= rect.y + rect.h {
                break;
            }
            ctx.print_color(rect.x, y, color, RGB::named((0x00, 0x00, 0x00)), text);
            y += 1;
        }
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
            w: w as i32 - DESCRIPTION_WIDTH,
            h: h as i32 - 2,
        };
        map_rect
    }

    fn get_mob_description_rect(ctx: &BTerm) -> Rect {
        let (w, _) = ctx.get_char_size();
        let map_rect = Rect {
            x: w as i32 - DESCRIPTION_WIDTH,
            y: 0,
            w: DESCRIPTION_WIDTH,
            h: MOB_DESCRIPTION_HEIGHT as i32,
        };
        map_rect
    }

    fn get_sensing_rect(ctx: &BTerm) -> Rect {
        let (w, _) = ctx.get_char_size();
        let map_rect = Rect {
            x: w as i32 - DESCRIPTION_WIDTH,
            y: MOB_DESCRIPTION_HEIGHT,
            w: DESCRIPTION_WIDTH,
            h: SENSING_HEIGHT,
        };
        map_rect
    }

    fn get_announcement_rect(ctx: &BTerm) -> Rect {
        let (w, h) = ctx.get_char_size();
        let map_rect = Rect {
            x: w as i32 - DESCRIPTION_WIDTH,
            y: MOB_DESCRIPTION_HEIGHT + SENSING_HEIGHT,
            w: DESCRIPTION_WIDTH,
            h: h as i32 - 2,
        };
        map_rect
    }

    fn draw(&mut self, ctx: &mut BTerm) {
        let (w, h) = ctx.get_char_size();
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
            i if i < PLAYER_MAX_HEALTH => RGB::named(LIGHT_YELLOW),
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

        let memory_rect = Self::get_mob_description_rect(ctx);
        let sensing_rect = Self::get_sensing_rect(ctx);
        let announcement_rect = Self::get_announcement_rect(ctx);

        ctx.print_color(
            memory_rect.x,
            memory_rect.y,
            RGB::named(LIGHT_WHITE),
            RGB::named(DARK_BLUE),
            format!("{:width$}", "RECALL", width = DESCRIPTION_WIDTH as usize),
        );

        fn desc_wrap(txt: &str) -> Vec<String> {
            textwrap::wrap(txt, DESCRIPTION_WIDTH as usize)
                .iter()
                .map(|txt| String::from(txt.clone()))
                .collect::<Vec<_>>()
        };

        let mut time_rng = SmallRng::seed_from_u64(Local::now().second().into());
        let mob_text = self.gs.get_mob_text(&mut time_rng);
        self.print_rect(
            ctx,
            memory_rect,
            &desc_wrap(&mob_text),
            RGB::named(LIGHT_WHITE),
            1,
        );

        ctx.print_color(
            sensing_rect.x,
            sensing_rect.y,
            RGB::named(LIGHT_WHITE),
            RGB::named(DARK_GREEN),
            format!(
                "{:width$}",
                "PERCEPTION",
                width = DESCRIPTION_WIDTH as usize
            ),
        );

        let memory_messages = self
            .gs
            .sensing
            .iter()
            .map(|s| desc_wrap(&s))
            .flatten()
            .map(|s| s.clone())
            .collect::<Vec<_>>();
        self.print_rect(
            ctx,
            memory_rect,
            &memory_messages,
            RGB::named(LIGHT_WHITE),
            1,
        );

        ctx.print_color(
            announcement_rect.x,
            announcement_rect.y,
            RGB::named(LIGHT_WHITE),
            RGB::named(DARK_RED),
            format!("{:width$}", "INTERCOM", width = DESCRIPTION_WIDTH as usize),
        );

        let announcement_messages = self
            .gs
            .announcements
            .iter()
            .map(|s| desc_wrap(&s))
            .flatten()
            .map(|s| s.clone())
            .collect::<Vec<_>>();
        self.print_rect(
            ctx,
            announcement_rect,
            &announcement_messages,
            RGB::named(LIGHT_WHITE),
            1,
        );

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
    let dt = ctx.frame_time_ms / 1000.0;
    if ctx.key.is_some() && ui.gs.player_is_dead() {
        ui.random_text_effect(ctx, "DEAD".to_string(), 1f32, RGB::named(DARK_RED));
        return;
    }
    if ctx.key.is_some() && matches!(ui.gs.state, world::MissionState::Win) {
        ui.random_text_effect(ctx, "YOU WIN".to_string(), 1f32, RGB::named(LIGHT_GREEN));
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
                    Space | Period | NumpadDecimal => true,
                    F7 => {
                        ui.gs.debug_mode = !ui.gs.debug_mode;
                        false
                    }
                    F8 => {
                        ui.gs
                            .world
                            .move_player(ui.gs.world.thing.pos - ui.gs.world.player_pos(), true);
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
    let pos_effects = ui.gs.tick(dt, moved, &mut ui.rng);
    for (pos, effect) in pos_effects {
        let pos = ui.map_to_screen(pos, Ui::get_map_rect(ctx));
        match effect {
            world::Effect::Creak => {
                ui.add_text_effect_1s(pos, "*creak*", RGB::named(DARK_RED));
            }
            world::Effect::Shuffle => {
                ui.add_text_effect_1s(pos, "*shuffle*", RGB::named(DARK_YELLOW));
            }
            world::Effect::Hiss => {
                ui.add_text_effect_1s(pos, "*hiss*", RGB::named(DARK_GREEN));
            }
            world::Effect::Scrape => {
                ui.add_text_effect_1s(pos, "*scrape*", RGB::named(DARK_RED));
            }
            world::Effect::Sing => {
                if ui.rng.gen::<f32>() < 0.5 {
                    ui.add_text_effect_1s(pos, "*whistle*", RGB::named(DARK_BLUE));
                } else {
                    let lyrics = [
                        "once again",
                        "you are my lucky star",
                        "in trouble",
                        "my lucky, lucky star",
                        "the window frames",
                        "putting on a smile",
                        "living in a *whistle* house",
                        "that was a strange mistake",
                        "only, only, only",
                        "someone's listening in",
                        "do not throw stones",
                        "red, green, blue",
                    ];
                    let line = lyrics.choose(&mut ui.rng).unwrap();
                    ui.add_text_effect_1s(
                        pos,
                        line,
                        RGB::named(DARK_BLUE),
                    );
                }
            }
        }
    }
}

fn new_game() -> world::GameState {
    let mut gs = world::GameState::new();
    gs.generate_world(RandomNumberGenerator::new().rand());
    gs
}

fn main() {
    let context = BTermBuilder::simple80x50()
        .with_title("Run")
        .with_fps_cap(30.0)
        .build()
        .unwrap();
    let ui = Ui {
        gs: new_game(),
        effects: vec![],
        rng: SmallRng::seed_from_u64(72),
    };
    main_loop(context, ui).unwrap();
}
