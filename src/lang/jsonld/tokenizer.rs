use std::{fmt::Display, num::ParseIntError, str::FromStr};

use chumsky::prelude::*;

use crate::{
    lang::Token,
    model::{spanned, Spanned},
};
use enum_methods::{EnumIntoGetters, EnumIsA, EnumToGetters};

#[derive(
    Clone, Debug, PartialEq, PartialOrd, Eq, Hash, EnumIntoGetters, EnumIsA, EnumToGetters,
)]
pub enum JsonToken {
    /// ,
    Comma,
    /// :
    Colon,
    /// [
    SqOpen,
    /// ]
    SqClose,
    /// {
    CuOpen,
    /// }
    CuClose,
    /// true
    True,
    /// false
    False,
    /// null
    Null,
    /// "String"
    String(String),
    /// 42
    Num(u32, Option<u32>),
}

impl Token for JsonToken {
    fn token(&self) -> Option<lsp_types::SemanticTokenType> {
        use lsp_types::SemanticTokenType;
        match self {
            JsonToken::True => Some(SemanticTokenType::ENUM_MEMBER),
            JsonToken::False => Some(SemanticTokenType::ENUM_MEMBER),
            JsonToken::Null => Some(SemanticTokenType::ENUM_MEMBER),
            JsonToken::String(_) => Some(SemanticTokenType::STRING),
            JsonToken::Num(_, _) => Some(SemanticTokenType::NUMBER),
            _ => None,
        }
    }
}

impl Display for JsonToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsonToken::Comma => write!(f, ","),
            JsonToken::Colon => write!(f, ":"),
            JsonToken::SqOpen => write!(f, "["),
            JsonToken::SqClose => write!(f, "]"),
            JsonToken::CuOpen => write!(f, "{{"),
            JsonToken::CuClose => write!(f, "}}"),
            JsonToken::True => write!(f, "true"),
            JsonToken::False => write!(f, "false"),
            JsonToken::Null => write!(f, "null"),
            JsonToken::String(x) => write!(f, "\"{}\"", x),
            JsonToken::Num(a, Some(b)) => write!(f, "{}.{}", a, b),
            JsonToken::Num(a, None) => write!(f, "{}", a),
        }
    }
}

pub fn tokenize(st: &str) -> (Vec<Spanned<JsonToken>>, Vec<Simple<char>>) {
    let parser = parser()
        .then_ignore(end().recover_with(skip_then_retry_until([])))
        .padded();

    let (json, errs) = parser.parse_recovery(st);

    (json.unwrap_or_default(), errs)
}

fn parser() -> impl Parser<char, Vec<Spanned<JsonToken>>, Error = Simple<char>> {
    let tok = just("true")
        .to(JsonToken::True)
        .or(just("false").to(JsonToken::False))
        .or(just("null").to(JsonToken::Null))
        .or(just(']').to(JsonToken::SqClose))
        .or(just('{').to(JsonToken::CuOpen))
        .or(just('}').to(JsonToken::CuClose))
        .or(just(':').to(JsonToken::Colon))
        .or(just(',').to(JsonToken::Comma))
        .or(just('[').to(JsonToken::SqOpen));

    let items = tok
        .or(parse_num().map(|(x, y)| JsonToken::Num(x, y)))
        .or(parse_string().map(JsonToken::String));

    items.map_with_span(spanned).padded().repeated()
}

fn parse_num() -> impl Parser<char, (u32, Option<u32>), Error = Simple<char>> {
    let frac = just('.').chain(text::digits(10));

    // let exp = just('e')
    //     .or(just('E'))
    //     .chain(just('+').or(just('-')).or_not())
    //     .chain::<char, _, _>(text::digits(10));

    let number = just('-')
        .or_not()
        .chain::<char, _, _>(text::int(10))
        .then(frac.or_not())
        .map(|(a, b)| {
            let a = u32::from_str(&a.into_iter().collect::<String>())?;
            let b = if let Some(b) = b {
                Some(u32::from_str(&b.into_iter().collect::<String>())?)
            } else {
                None
            };

            Ok::<(u32, Option<u32>), ParseIntError>((a, b))
        })
        .unwrapped()
        .labelled("number");

    number
}

fn parse_string() -> impl Parser<char, String, Error = Simple<char>> {
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t'))
            .or(just('u').ignore_then(
                filter(|c: &char| c.is_digit(16))
                    .repeated()
                    .exactly(4)
                    .collect::<String>()
                    .validate(|digits, span, emit| {
                        char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(
                            || {
                                emit(Simple::custom(span, "invalid unicode character"));
                                '\u{FFFD}' // unicode replacement character
                            },
                        )
                    }),
            )),
    );

    just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .labelled("string")
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::JsonToken::*;

    #[test]
    fn parse_simple() {
        let (tokens, errs) = tokenize("");
        assert!(tokens.is_empty());
        assert!(errs.is_empty());

        let (tokens, errs) = tokenize(", [ ] { } null true false");
        let tokens: Vec<_> = tokens.into_iter().map(|x| x.into_value()).collect();
        assert_eq!(
            tokens,
            vec![Comma, SqOpen, SqClose, CuOpen, CuClose, Null, True, False]
        );
        assert!(errs.is_empty());
    }

    #[test]
    fn parse_string() {
        let (tokens, errs) = tokenize(" \"Epic string!!\"");
        let tokens: Vec<_> = tokens.into_iter().map(|x| x.into_value()).collect();
        assert_eq!(tokens, vec![String("Epic string!!".into())]);
        assert!(errs.is_empty());

        let (tokens, errs) = tokenize(" \"Epic string!!");
        let tokens: Vec<_> = tokens.into_iter().map(|x| x.into_value()).collect();
        assert_eq!(tokens, vec![]);
        assert_eq!(errs.len(), 1);
    }

    #[test]
    fn parse_num() {
        let (tokens, errs) = tokenize(" 423");
        let tokens: Vec<_> = tokens.into_iter().map(|x| x.into_value()).collect();
        assert_eq!(tokens, vec![Num(423, None)]);
        assert!(errs.is_empty());
    }
}
