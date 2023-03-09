use std::ops::Range;

pub mod contexts;
pub mod model;
pub mod parser;
pub mod semantics;
pub mod turtle;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Error {
    pub msg: String,
    pub span: Range<usize>,
}
