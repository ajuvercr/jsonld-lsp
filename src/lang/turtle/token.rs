use enum_methods::{EnumIntoGetters, EnumIsA, EnumToGetters};

#[derive(Clone, PartialEq, Debug, EnumIntoGetters, EnumIsA, EnumToGetters)]
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
    PNameNS(Option<String>),
    PNameLN(Option<String>, String),
    /// _:...
    BlankNodeLabel(String),
    /// @...
    LangTag(String),
    Number(String),

    /// '...'
    StringSingleQuote(String),
    /// "..."
    StringDoubleQuote(String),
    /// '''...'''
    LongStringSingleQuote(String),
    /// """..."""
    LongStringDoubleQuote(String),

    /// [ ]
    ANON,
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
            Token::PNameNS(_) => write!(f, "a prefix"),
            Token::PNameLN(_, _) => write!(f, "a prefixed node"),
            Token::BlankNodeLabel(_) => write!(f, "a blank node"),
            Token::LangTag(_) => write!(f, "a language tag"),
            Token::Number(_) => write!(f, "a number"),
            Token::StringSingleQuote(_) => write!(f, "a string"),
            Token::StringDoubleQuote(_) => write!(f, "a string"),
            Token::LongStringSingleQuote(_) => write!(f, "a string"),
            Token::LongStringDoubleQuote(_) => write!(f, "a string"),
            Token::ANON => write!(f, "an inline blank node"),
        }
    }
}
