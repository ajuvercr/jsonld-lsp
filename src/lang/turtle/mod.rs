mod formatter;
mod model;
mod node;
mod parser;
mod token;
pub mod tokenizer;
use lsp_types::{FormattingOptions, Position, SemanticTokenType};
use std::ops::Range;

use chumsky::{prelude::Simple, primitive::end, recovery::skip_then_retry_until, Parser};
pub use model::*;

pub use parser::*;
use ropey::Rope;

use crate::{
    lang,
    model::{spanned, Spanned},
    parent::ParentingSystem,
    semantics::semantic_tokens,
};

use self::{
    formatter::format_turtle,
    node::{Leaf, Node},
};

use super::{CurrentLangState, Lang, LangState};

pub mod semantic_token {
    use lsp_types::SemanticTokenType as STT;
    pub const BOOLEAN: STT = STT::new("boolean");
    pub const LANG_TAG: STT = STT::new("langTag");
}

#[allow(unused)]
pub struct TurtleLang {
    id: String,
    rope: Rope,
    comments: Vec<Spanned<String>>,
}

impl Lang for TurtleLang {
    type State = ();

    type Token = token::Token;

    type TokenError = Simple<char>;

    type Element = model::Turtle;

    type ElementError = Simple<token::Token>;

    type RenameError = ();

    type PrepareRenameError = ();

    type Node = Node;
    type NodeLeaf = Leaf;

    const LANG: &'static str = "turtle";

    const TRIGGERS: &'static [&'static str] = &[];

    const LEGEND_TYPES: &'static [lsp_types::SemanticTokenType] = &[
        semantic_token::BOOLEAN,
        semantic_token::LANG_TAG,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::NUMBER,
        SemanticTokenType::STRING,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::ENUM,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::COMMENT,
    ];

    fn pattern() -> Option<String> {
        Some(String::from("*.{ttl,turtle}"))
    }

    fn format(
        &mut self,
        state: &CurrentLangState<Self>,
        options: FormattingOptions,
    ) -> Option<String> {
        let stream = chumsky::Stream::from_iter(
            0..self.rope.len_chars() + 1,
            state
                .tokens
                .current
                .iter()
                .filter(|x| !x.is_comment())
                .map(|Spanned(x, s)| (x.clone(), s.clone())),
        );

        let parser = turtle()
            .map_with_span(spanned)
            .then_ignore(end().recover_with(skip_then_retry_until([])));

        let turtle = parser.parse(stream).ok()?;
        format_turtle(&turtle.0, options, &self.comments, &self.rope)
    }

    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>) {
        self.rope = Rope::from(source.to_owned());
        let parser = tokenizer::parse_tokens();

        let (json, errs) = parser.parse_recovery(source);

        let tokens: Vec<_> = json
            .unwrap_or_default()
            .into_iter()
            .map(|(x, y)| spanned(x, y))
            .collect();

        (tokens, errs)
    }

    fn parse(
        &mut self,
        source: &str,
        tokens: &Vec<Spanned<Self::Token>>,
    ) -> (Spanned<Self::Element>, Vec<Self::ElementError>) {
        let tokens = tokens.clone();
        let mut comments: Vec<_> = tokens
            .iter()
            .filter(|x| x.is_comment())
            .cloned()
            .map(|x| x.map(|x| x.to_comment()))
            .collect();
        comments.sort_by_key(|x| x.1.start);
        self.comments = comments;

        let stream = chumsky::Stream::from_iter(
            0..source.len() + 1,
            tokens
                .into_iter()
                .filter(|x| !x.is_comment())
                .map(|Spanned(x, s)| (x, s)),
        );

        let parser = turtle()
            .map_with_span(spanned)
            .then_ignore(end().recover_with(skip_then_retry_until([])));
        let (json, json_errors) = parser.parse_recovery(stream);

        (
            json.unwrap_or(Spanned(Default::default(), 0..source.len())),
            json_errors,
        )
    }

    fn parents(&self, _element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>> {
        ParentingSystem::new()
        // let p = ParentingSystem::new_turtle(element.clone());
        //
        // p
    }

    fn special_semantic_tokens(
        &self,
        state: &CurrentLangState<Self>,
        mut apply: impl FnMut(Range<usize>, lsp_types::SemanticTokenType) -> (),
    ) {
        use lang::Token;
        state.tokens.current.iter().for_each(|token| {
            Token::span_tokens(token)
                .into_iter()
                .for_each(|(token, span)| apply(span, token))
        });
    }

    fn prepare_rename(
        &self,
        _state: &CurrentLangState<Self>,
        _pos: usize,
    ) -> Result<(Range<usize>, String), Self::PrepareRenameError> {
        Err(())
    }

    fn rename(
        &self,
        _state: &CurrentLangState<Self>,
        _pos: usize,
        _new_name: String,
    ) -> Result<Vec<(Range<usize>, String)>, Self::RenameError> {
        Err(())
    }

    fn new(id: String, _state: Self::State) -> (Self, CurrentLangState<Self>) {
        (
            TurtleLang {
                id,
                rope: Rope::new(),
                comments: Vec::new(),
            },
            Default::default(),
        )
    }
}

#[async_trait::async_trait]
impl LangState for TurtleLang {
    async fn update(&mut self, _state: &CurrentLangState<Self>) {}

    async fn do_semantic_tokens(
        &mut self,
        state: &CurrentLangState<Self>,
    ) -> Vec<lsp_types::SemanticToken> {
        semantic_tokens(self, state, &self.rope)
    }

    async fn do_completion(
        &mut self,
        _trigger: Option<String>,
        _position: &Position,
        _state: &CurrentLangState<Self>,
    ) -> Vec<super::SimpleCompletion> {
        Vec::new()
    }
}
