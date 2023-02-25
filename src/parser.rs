use chumsky::prelude::*;
use std::ops::Range;

use crate::model::{spanned, Json, Obj, Spanned};

pub fn parse(source: &str) -> (Spanned<Json>, Vec<Error>) {
    let (json, errs) = parser().parse_recovery(source);
    let errors = errs
        .into_iter()
        .map(|e| {
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
            Error {
                msg,
                span: e.span(),
            }
        })
        .collect();

    (
        json.unwrap_or_else(|| Spanned(Json::Invalid, 0..source.len())),
        errors,
    )
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Error {
    pub msg: String,
    pub span: Range<usize>,
}

#[cfg(test)]
mod tests {
    use super::parse;

    #[test]
    fn test_it() {
        let json = r#"
{ "@context": "https://data.vlaanderen.be/doc/applicatieprofiel/sensoren-en-bemonstering/kandidaatstandaard/2022-04-28/context/ap-sensoren-en-bemonstering.jsonld", "@id": "tetten"         ,     }
            "#;
        let (json, _) = parse(json);

        let array = json.iter().count();
        assert_eq!(array, 1);

        let json = r#"
{ "@context": "https://data.vlaanderen.be/doc/applicatieprofiel/sensoren-en-bemonstering/kandidaatstandaard/2022-04-28/context/ap-sensoren-en-bemonstering.jsonld", "@id": "tetten"}
            "#;
        let (json, _) = parse(json);

        let array = json.iter().count();
        assert_eq!(array, 3);
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
            .padded()
            .delimited_by(just('['), just(']'))
            .map(Json::Array)
            .labelled("array");

        let member = string
            .clone()
            .map_with_span(spanned)
            .then_ignore(just(':').padded())
            .then(value)
            .map_with_span(spanned);

        let object = member
            .clone()
            .chain(just(',').padded().ignore_then(member).repeated())
            .or_not()
            .flatten()
            .padded()
            .delimited_by(just('{'), just('}'))
            .collect::<Vec<_>>()
            .map(|arr| Json::Object(Obj(arr)))
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
    .then_ignore(end().recover_with(skip_then_retry_until([])))
    .padded()
}
