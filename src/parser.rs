use chumsky::prelude::*;
use std::{
    collections::HashMap,
    ops::{Deref, Range}, borrow::Borrow,
};


pub fn parse(source: &str) -> (Spanned<Json>, Vec<Error>) {
    let (json, errs) = parser().parse_recovery(source);
    let errors = errs.into_iter().map(|e| {
        let msg = if let chumsky::error::SimpleReason::Custom(msg) = e.reason() {
            msg.clone()
        } else {
            format!(
                "{}{}, expected {}",
                if e.found().is_some() {
                    "Unexpected token"
                } else {
                    "Unexpected end of input"
                },
                if let Some(label) = e.label() {
                    format!(" while parsing {}", label)
                } else {
                    String::new()
                },
                if e.expected().len() == 0 {
                    "something else".to_string()
                } else {
                    e.expected()
                        .map(|expected| match expected {
                            Some(expected) => format!("'{}'", expected),
                            None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(" or ")
                },
            )
        };
        Error { msg, span: e.span()}
    }).collect();

    (
        json.unwrap_or_else(|| Spanned(Json::Invalid, 0..source.len())),
        errors
    )
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Error {
    pub msg: String,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct Spanned<T>(T, Range<usize>);
impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T: std::hash::Hash> std::hash::Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl<T: PartialEq> Eq for Spanned<T> {}

fn spanned<T>(t: T, span: Range<usize>) -> Spanned<T> {
    Spanned(t, span)
}

impl Borrow<str> for Spanned<String> {
    #[inline]
    fn borrow(&self) -> &str {
        &self[..]
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Spanned<T> {
    pub fn into_value(self) -> T {
        self.0
    }
    pub fn into_span(self) -> Range<usize> {
        self.1
    }
    pub fn value(&self) -> &T {
        &self.0
    }
    pub fn span(&self) -> &Range<usize> {
        &self.1
    }
}

#[derive(Clone, Debug)]
pub enum Json {
    Invalid,
    Null,
    Bool(bool),
    Str(String),
    Num(f64),
    Array(Vec<Spanned<Json>>),
    Object(Obj),
}
pub type Obj = HashMap<Spanned<String>, Spanned<Json>>;

#[allow(unused)]
impl Json {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Json::Bool(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Json::Str(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_num(&self) -> Option<f64> {
        match self {
            Json::Num(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_arr(&self) -> Option<&[Spanned<Json>]> {
        match self {
            Json::Array(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_obj(&self) -> Option<&Obj> {
        match self {
            Json::Object(x) => Some(x),
            _ => None,
        }
    }
}

// Source: https://github.com/zesterer/chumsky/blob/master/examples/json.rs
fn parser() -> impl Parser<char, Spanned<Json>, Error = Simple<char>> {
    recursive(|value| {
        let frac = just('.').chain(text::digits(10));

        let exp = just('e')
            .or(just('E'))
            .chain(just('+').or(just('-')).or_not())
            .chain::<char, _, _>(text::digits(10));

        let number = just('-')
            .or_not()
            .chain::<char, _, _>(text::int(10))
            .chain::<char, _, _>(frac.or_not().flatten())
            .chain::<char, _, _>(exp.or_not().flatten())
            .collect::<String>()
            .from_str()
            .unwrapped()
            .labelled("number");

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
                            char::from_u32(u32::from_str_radix(&digits, 16).unwrap())
                                .unwrap_or_else(|| {
                                    emit(Simple::custom(span, "invalid unicode character"));
                                    '\u{FFFD}' // unicode replacement character
                                })
                        }),
                )),
        );

        let string = just('"')
            .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
            .then_ignore(just('"'))
            .collect::<String>()
            .labelled("string");

        let array = value
            .clone()
            .chain(just(',').ignore_then(value.clone()).repeated())
            .or_not()
            .flatten()
            .delimited_by(just('['), just(']'))
            .map(Json::Array)
            .labelled("array");

        let member = string
            .clone()
            .map_with_span(spanned)
            .then_ignore(just(':').padded())
            .then(value);
        let object = member
            .clone()
            .chain(just(',').padded().ignore_then(member).repeated())
            .or_not()
            .flatten()
            .padded()
            .delimited_by(just('{'), just('}'))
            .collect::<HashMap<Spanned<String>, Spanned<Json>>>()
            .map(Json::Object)
            .labelled("object");

        just("null")
            .to(Json::Null)
            .labelled("null")
            .or(just("true").to(Json::Bool(true)).labelled("true"))
            .or(just("false").to(Json::Bool(false)).labelled("false"))
            .or(number.map(Json::Num))
            .or(string.map(Json::Str))
            .or(array)
            .or(object)
            .recover_with(nested_delimiters('{', '}', [('[', ']')], |_| Json::Invalid))
            .recover_with(nested_delimiters('[', ']', [('{', '}')], |_| Json::Invalid))
            .recover_with(skip_then_retry_until(['}', ']']))
            .padded()
            .map_with_span(spanned)
    })
    .then_ignore(end().recover_with(skip_then_retry_until([]))).padded()
}

