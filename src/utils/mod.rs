mod loader;
use std::{fmt::Display, fs::File, sync::Mutex};

pub use loader::*;

mod fetch;
pub use fetch::*;

pub mod web_types;

use crate::lsp_types::{Position, Range};
use ropey::Rope;

static mut LOG_FILE: Option<Mutex<File>> = None;

pub fn init_log() {
    let file = File::options()
        .write(true)
        .create(true)
        .open("/tmp/jsonld-log.txt")
        .unwrap();
    unsafe { LOG_FILE = Some(Mutex::new(file)) };
}

pub fn log(msg: impl Display) {
    use std::io::Write;

    unsafe {
        if let Some(ref file) = LOG_FILE {
            if let Ok(mut f) = file.lock() {
                write!(f, "{}\n", msg).unwrap();
            }
        }
    }
}

pub fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}
pub fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_offset = rope.try_line_to_char(position.line as usize).ok()?;
    let line_length = rope.line(position.line as usize).len_chars();

    if (position.character as usize) < line_length {
        Some(line_offset + position.character as usize)
    } else {
        None
    }
}
pub fn offsets_to_range(start: usize, end: usize, rope: &Rope) -> Option<Range> {
    let start = offset_to_position(start, rope)?;
    let end = offset_to_position(end, rope)?;
    Some(Range { start, end })
}
