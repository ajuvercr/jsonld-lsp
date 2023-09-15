pub mod tokenizer;
// pub mod parser;
mod model;
mod parser;
mod token;
use std::ops::Range;

use chumsky::prelude::Simple;
pub use model::*;

pub use parser::*;
use ropey::Rope;

use crate::{model::Spanned, parent::ParentingSystem};

use super::Lang;

pub struct TurtleLang {
    id: String,
    rope: Rope,
}

impl Lang for TurtleLang {
    type State = ();

    type Token = token::Token;

    type TokenError = Simple<char>;

    type Element = model::Turtle;

    type ElementError = Simple<token::Token>;

    type RenameError = ();

    type PrepareRenameError = ();

    type Node = ();

    const LANG: &'static str = "turtle";

    const TRIGGERS: &'static [&'static str] = &[];

    const LEGEND_TYPES: &'static [lsp_types::SemanticTokenType] = &[];

    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>) {
        todo!()
    }

    fn parse(
        &self,
        source: &str,
        tokens: &Vec<Spanned<Self::Token>>,
    ) -> (Spanned<Self::Element>, Vec<Self::ElementError>) {
        todo!()
    }

    fn parents(&self, element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>> {
        todo!()
    }

    fn semantic_tokens(
        &self,
        system: &ParentingSystem<Spanned<Self::Node>>,
        apply: impl FnMut(Range<usize>, lsp_types::SemanticTokenType) -> (),
    ) {
        todo!()
    }

    fn prepare_rename(
        &self,
        pos: usize,
    ) -> Result<(Range<usize>, String), Self::PrepareRenameError> {
        todo!()
    }

    fn rename(
        &self,
        pos: usize,
        new_name: String,
    ) -> Result<Vec<(Range<usize>, String)>, Self::RenameError> {
        todo!()
    }

    fn new(id: String, state: Self::State) -> Self {
        TurtleLang {
            id,
            rope: Rope::new(),
        }
    }
}
