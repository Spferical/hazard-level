use bracket_lib::prelude::*;

use std::ops::{Add, AddAssign, Sub};

struct Pos(i32, i32);
struct Off(i32, i32);

impl Add<Off> for Pos {
    type Output = Pos;

    fn add(self, offset: Off) -> Pos {
        Pos(self.0 + offset.0, self.1 + offset.1)
    }
}

impl Sub<Pos> for Pos {
    type Output = Off;

    fn sub(self, other: Pos) -> Off {
        Off(self.0 - other.0, self.1 - other.1)
    }
}

impl AddAssign<Off> for Pos {
    fn add_assign(&mut self, o: Off) {
        self.0 += o.0;
        self.1 += o.1;
    }
}

struct State {
    player_pos: Pos,
}

impl GameState for State {
    fn tick(&mut self, ctx: &mut BTerm) {
        player_input(self, ctx);
        ctx.cls();
        ctx.print(1, 1, "Hello Rust World");
        let Pos(x, y) = self.player_pos;
        ctx.print(x, y, "@");
    }
}

impl State {
    fn move_player(&mut self, off: Off) {
        self.player_pos += off;
    }
}

fn player_input(gs: &mut State, ctx: &mut BTerm) {
    // Player movement
    match ctx.key {
        None => {} // Nothing happened
        Some(key) => match key {
            VirtualKeyCode::Left => gs.move_player(Off(-1, 0)),
            VirtualKeyCode::Right => gs.move_player(Off(1, 0)),
            VirtualKeyCode::Up => gs.move_player(Off(0, -1)),
            VirtualKeyCode::Down => gs.move_player(Off(0, 1)),
            _ => {}
        },
    }
}

fn main() {
    let context = BTermBuilder::simple80x50()
        .with_title("Roguelike Tutorial")
        .build();
    let gs = State {
        player_pos: Pos(0, 0),
    };
    main_loop(context, gs);
}
