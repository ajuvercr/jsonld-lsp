use std::ops::Range;

use chumsky::{prelude::*, primitive::Container, Stream};
use enum_methods::{EnumIntoGetters, EnumIsA, EnumToGetters};

use crate::model::{spanned, Spanned};

use super::tokenizer::JsonToken;

#[derive(Clone, PartialEq, Debug, EnumIntoGetters, EnumIsA, EnumToGetters)]
pub enum Json {
    Invalid,
    Token(JsonToken),
    Array(Vec<Spanned<Json>>),
    Object(Vec<Spanned<(Spanned<String>, Spanned<Json>)>>),
}

pub fn parse(
    source: &str,
    tokens: Vec<Spanned<JsonToken>>,
) -> (Spanned<Json>, Vec<Simple<JsonToken>>) {
    let stream = Stream::from_iter(
        0..source.len(),
        tokens.into_iter().map(|Spanned(x, s)| (x, s)),
    );

    let parser = parser().then_ignore(end().recover_with(skip_then_retry_until([])));
    let (json, json_errors) = parser.parse_recovery(stream);

    (
        json.unwrap_or(Spanned(Json::Invalid, 0..source.len())),
        json_errors,
    )
}

fn munch<T: Clone, C: Container<JsonToken>>(
    c: C,
    default: impl Fn(Range<usize>) -> T + 'static,
) -> impl Parser<JsonToken, T, Error = Simple<JsonToken>> {
    let munch = none_of(c)
        .repeated()
        .map_with_span(move |_, span| default(span));

    munch
}

fn parser() -> impl Parser<JsonToken, Spanned<Json>, Error = Simple<JsonToken>> {
    use JsonToken::*;

    recursive(|value| {
        let array = value
            .clone()
            .recover_with(skip_parser(munch(&[Comma, CuClose], |span| {
                Spanned(Json::Invalid, span)
            })))
            .separated_by(just(Comma).recover_with(skip_then_retry_until([])))
            .allow_trailing()
            .delimited_by(just(SqOpen), just(SqClose))
            .map(Json::Array);

        let member = filter(JsonToken::is_string)
            .map(JsonToken::into_string)
            .map_with_span(spanned)
            .then_ignore(filter(JsonToken::is_colon))
            .then(value.clone())
            .recover_with(skip_then_retry_until([]));

        let obj = member
            .map_with_span(spanned)
            .separated_by(just(Comma).recover_with(skip_then_retry_until([])))
            .delimited_by(just(CuOpen), just(CuClose))
            .map(Json::Object);

        let null = just(Null).map(Json::Token);
        let t = just(True).map(Json::Token);
        let f = just(False).map(Json::Token);
        let st = filter(JsonToken::is_string).map(Json::Token);
        let num = filter(JsonToken::is_num).map(Json::Token);

        choice((null, t, f, st, num, obj, array)).map_with_span(spanned)
    })
}

#[cfg(test)]
mod tests {

    use crate::lang::jsonld::tokenizer::tokenize;

    use super::*;

    #[test]
    fn parse_json_simple() {
        let source = "\"test\"";
        let (tokens, token_errors) = tokenize(source);
        let (json, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        assert!(json_errors.is_empty());

        assert_eq!(
            json.into_value(),
            Json::Token(JsonToken::String("test".into()))
        );
    }

    #[test]
    fn parse_json_array() {
        let source = "[\"test\", 42]";
        let (tokens, token_errors) = tokenize(source);
        println!("Tokens {:?}", tokens);
        let (json, json_errors) = parse(source, tokens);
        println!("Json {:?}", json);

        assert!(token_errors.is_empty());
        assert!(json_errors.is_empty());

        let arr: Vec<_> = match json.into_value() {
            Json::Array(x) => x.into_iter().map(|x| x.into_value()).collect(),
            _ => panic!("Expected json array"),
        };

        assert_eq!(
            arr,
            vec![
                Json::Token(JsonToken::String("test".into())),
                Json::Token(JsonToken::Num(42, 0))
            ]
        );
    }

    #[test]
    fn parse_json_array_invalid() {
        let source = "[\"test\" : , 42]";
        let (tokens, token_errors) = tokenize(source);
        let (json, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        assert_eq!(json_errors.len(), 1);

        let arr: Vec<_> = match json.into_value() {
            Json::Array(x) => x.into_iter().map(|x| x.into_value()).collect(),
            _ => panic!("Expected json array"),
        };

        assert_eq!(
            arr,
            vec![
                Json::Token(JsonToken::String("test".into())),
                Json::Token(JsonToken::Num(42, 0)),
            ]
        );
    }
}
