use std::ops::Range;

use enum_methods::{EnumIntoGetters, EnumIsA, EnumToGetters};

use crate::{lang, model::Spanned};

use super::semantic_token;

#[derive(Clone, PartialEq, Eq, Hash, Debug, EnumIntoGetters, EnumIsA, EnumToGetters)]
pub enum Token {
    /// @prefix
    PrefixTag,
    /// @base
    BaseTag,
    /// sparql prefix
    SparqlPrefix,
    /// sparql base
    SparqlBase,
    /// a
    PredType,

    /// [
    BNodeStart,
    /// ]
    BNodeEnd,
    /// (
    ColStart,
    /// )
    ColEnd,

    /// ^^
    DataTypeDelim,

    /// .
    Stop,
    /// ;
    PredicateSplit,
    /// ,
    ObjectSplit,

    /// true
    True,
    /// false
    False,
    /// <...>
    IRIRef(String),

    /// ..:
    PNameLN(Option<String>, String),
    /// _:...
    BlankNodeLabel(String),
    /// @...
    LangTag(String),

    Number(String),
    /// All string types
    Str(String, StringStyle),

    /// [ ]
    ANON,
    Comment(String),

    Invalid(String),
}

impl lang::Token for Token {
    fn token(&self) -> Option<lsp_types::SemanticTokenType> {
        match self {
            Token::PrefixTag
            | Token::BaseTag
            | Token::SparqlPrefix
            | Token::SparqlBase
            | Token::PredType => Some(lsp_types::SemanticTokenType::KEYWORD),
            Token::True | Token::False => Some(semantic_token::BOOLEAN),
            Token::IRIRef(_) => Some(lsp_types::SemanticTokenType::PROPERTY),
            Token::LangTag(_) => Some(semantic_token::LANG_TAG),
            Token::Number(_) => Some(lsp_types::SemanticTokenType::NUMBER),
            Token::Str(_, _) => Some(lsp_types::SemanticTokenType::STRING),
            Token::Comment(_) => Some(lsp_types::SemanticTokenType::COMMENT),
            _ => None,
        }
    }

    fn span_tokens(
        Spanned(this, span): &Spanned<Self>,
    ) -> Vec<(lsp_types::SemanticTokenType, Range<usize>)> {
        if let Some(t) = this.token() {
            return vec![(t, span.clone())];
        }

        match this {
            Token::PNameLN(p, _) => {
                let s = p.as_ref().map(|x| x.len()).unwrap_or(0);

                vec![
                    (
                        lsp_types::SemanticTokenType::NAMESPACE,
                        span.start..span.start + 1 + s,
                    ),
                    (
                        lsp_types::SemanticTokenType::ENUM_MEMBER,
                        span.start + s + 1..span.end,
                    ),
                ]
            }
            Token::BlankNodeLabel(_) => {
                vec![
                    (
                        lsp_types::SemanticTokenType::NAMESPACE,
                        span.start..span.start + 2,
                    ),
                    (
                        lsp_types::SemanticTokenType::PROPERTY,
                        span.start + 2..span.end,
                    ),
                ]
            }
            _ => vec![],
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, EnumIntoGetters, EnumIsA, EnumToGetters)]
pub enum StringStyle {
    /// """..."""
    DoubleLong,
    /// "..."
    Double,
    /// '''...'''
    SingleLong,
    /// '...'
    Single,
}

impl StringStyle {
    pub fn quote(&self) -> &'static str {
        match self {
            StringStyle::DoubleLong => "\"\"\"",
            StringStyle::Double => "\"",
            StringStyle::SingleLong => "'''",
            StringStyle::Single => "'",
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::PrefixTag => write!(f, "'@prefix'"),
            Token::BaseTag => write!(f, "'@base'"),
            Token::SparqlPrefix => write!(f, "'PREFIX'"),
            Token::SparqlBase => write!(f, "'BASE'"),
            Token::PredType => write!(f, "'a'"),
            Token::BNodeStart => write!(f, "'['"),
            Token::BNodeEnd => write!(f, "']'"),
            Token::ColStart => write!(f, "'('"),
            Token::ColEnd => write!(f, "')'"),
            Token::DataTypeDelim => write!(f, "'^^'"),
            Token::Stop => write!(f, "'.'"),
            Token::PredicateSplit => write!(f, "';'"),
            Token::ObjectSplit => write!(f, "','"),
            Token::True => write!(f, "'true'"),
            Token::False => write!(f, "'false'"),
            Token::IRIRef(_) => write!(f, "a named node"),
            Token::PNameLN(_, _) => write!(f, "a prefixed node"),
            Token::BlankNodeLabel(_) => write!(f, "a blank node"),
            Token::LangTag(_) => write!(f, "a language tag"),
            Token::Number(_) => write!(f, "a number"),
            Token::Str(_, _) => write!(f, "a string"),
            Token::ANON => write!(f, "an inline blank node"),
            Token::Comment(_) => write!(f, "a comment"),
            Token::Invalid(_) => write!(f, "invalid token"),
        }
    }
}
