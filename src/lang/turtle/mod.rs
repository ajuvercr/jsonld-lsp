pub mod tokenizer;
// pub mod parser;
mod model;
mod node;
mod parser;
mod token;
use lsp_types::SemanticTokenType;
use std::ops::Range;
use token::Token;

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

use self::node::{Leaf, Node};

use super::{Lang, LangState};

pub mod semantic_token {
    use lsp_types::SemanticTokenType as STT;
    pub const BOOLEAN: STT = STT::new("boolean");
    pub const LANG_TAG: STT = STT::new("langTag");
}

pub struct TurtleLang {
    id: String,
    rope: Rope,
    tokens: Vec<Spanned<Token>>,
    parents: ParentingSystem<Spanned<<Self as Lang>::Node>>,
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
    ];

    fn tokenize(&mut self, source: &str) -> (Vec<Spanned<Self::Token>>, Vec<Self::TokenError>) {
        self.rope = Rope::from(source.to_owned());
        let parser = tokenizer::parse_tokens();

        let (json, errs) = parser.parse_recovery(source);

        let tokens: Vec<_> = json
            .unwrap_or_default()
            .into_iter()
            .map(|(x, y)| spanned(x, y))
            .collect();

        if errs.is_empty() {
            self.tokens = tokens.clone();
        }

        (tokens, errs)
    }

    fn parse(
        &self,
        source: &str,
        tokens: &Vec<Spanned<Self::Token>>,
    ) -> (Spanned<Self::Element>, Vec<Self::ElementError>) {
        let stream = chumsky::Stream::from_iter(
            0..source.len() + 1,
            tokens.clone().into_iter().map(|Spanned(x, s)| (x, s)),
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

    fn parents(&self, element: &Spanned<Self::Element>) -> ParentingSystem<Spanned<Self::Node>> {
        ParentingSystem::new()
        // let p = ParentingSystem::new_turtle(element.clone());
        //
        // p
    }

    fn special_semantic_tokens(
        &self,
        mut apply: impl FnMut(Range<usize>, lsp_types::SemanticTokenType) -> (),
    ) {
        use lang::Token;
        self.tokens.iter().for_each(|token| {
            Token::span_tokens(token)
                .into_iter()
                .for_each(|(token, span)| apply(span, token))
        });
    }

    fn prepare_rename(
        &self,
        pos: usize,
    ) -> Result<(Range<usize>, String), Self::PrepareRenameError> {
        Err(())
    }

    fn rename(
        &self,
        pos: usize,
        new_name: String,
    ) -> Result<Vec<(Range<usize>, String)>, Self::RenameError> {
        Err(())
    }

    fn new(id: String, state: Self::State) -> Self {
        TurtleLang {
            id,
            tokens: Vec::new(),
            rope: Rope::new(),
            parents: ParentingSystem::new(),
        }
    }
}

#[async_trait::async_trait]
impl LangState for TurtleLang {
    async fn update(&mut self, parents: ParentingSystem<Spanned<<TurtleLang as Lang>::Node>>) {
        self.parents = parents;
    }

    async fn do_semantic_tokens(&mut self) -> Vec<lsp_types::SemanticToken> {
        semantic_tokens(self, &self.parents, &self.rope)
    }

    async fn do_completion(&mut self, trigger: Option<String>) -> Vec<super::SimpleCompletion> {
        Vec::new()
    }
}
