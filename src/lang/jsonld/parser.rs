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
        0..source.len() + 1,
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
        .map(|x| {
            println!("MUNCHED {}", x);
            x
        })
        .repeated()
        .map_with_span(move |_, span| default(span));

    munch
}

fn parser() -> impl Parser<JsonToken, Spanned<Json>, Error = Simple<JsonToken>> {
    use JsonToken::*;

    recursive(|value| {
        let array = value
            .clone()
            .recover_with(skip_parser(
                none_of([SqClose]).to(Json::Invalid).map_with_span(spanned),
            ))
            .separated_by(just(Comma))
            .delimited_by(just(SqOpen), just(SqClose))
            .map(Json::Array);

        // let array = just(SqOpen).ignore_then(value.clone().separated_by(just(Comma))).then_ignore(just(SqClose)).map(Json::Array);

        let member = filter(JsonToken::is_string)
            .map(JsonToken::into_string)
            .map_with_span(spanned)
            .then_ignore(just(Colon))
            .then(value.clone());

        let obj = just(CuOpen)
            .ignore_then(
                member
                    .clone()
                    .map_with_span(spanned)
                    .separated_by(just(Comma)),
            )
            .then_ignore(just(CuClose))
            .map(Json::Object);
        // let obj = member
        //     .map_with_span(spanned)
        //     .separated_by(just(Comma).recover_with(skip_then_retry_until([])))
        //     .delimited_by(just(CuOpen), just(CuClose))
        //     .map(Json::Object);

        let null = just(Null).map(Json::Token);
        let t = just(True).map(Json::Token);
        let f = just(False).map(Json::Token);
        let st = filter(JsonToken::is_string).map(Json::Token);
        let num = filter(JsonToken::is_num).map(Json::Token);

        choice((st, array, obj, null, t, f, num)).map_with_span(spanned)
    })
}

#[cfg(test)]
mod tests {
    use crate::lang::jsonld::{parent, tokenizer::tokenize};

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
                Json::Token(JsonToken::Num(42, None))
            ]
        );
    }
    #[test]
    fn parse_json_array_to_vec() {
        let source = "[\"test\", 42]";
        let (tokens, token_errors) = tokenize(source);
        let (json, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        assert!(json_errors.is_empty());

        let parents = parent::system(json);

        let vec = parents.to_json_vec().unwrap();

        assert_eq!(vec, b"[\"test\",42]");

        let source = "{}";
        let (tokens, token_errors) = tokenize(source);
        let (json, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        assert!(json_errors.is_empty());

        let parents = parent::system(json);

        let vec = parents.to_json_vec().unwrap();

        assert_eq!(vec, b"{}");
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
                Json::Token(JsonToken::Num(42, None)),
            ]
        );
    }

    #[test]
    fn parse_failed() {
        let source = r#"
{
  "@context": [
    "https://data.vlaanderen.be/doc/applicatieprofiel/sensoren-en-bemonstering/kandidaatstandaard/2022-04-28/context/ap-sensoren-en-bemonstering.jsonld",
    {
      "foaf": "foaf_exp"
    } 
  ], "test": "test_exp"
}
"#;

        let (tokens, token_errors) = tokenize(source);

        println!(
            "tokens {:?}",
            tokens.iter().map(|x| x.value()).collect::<Vec<_>>()
        );
        let (json, json_errors) = parse(source, tokens);
        println!("json {:?}", json.0);

        assert!(token_errors.is_empty());
        assert_eq!(json_errors.len(), 0);
    }
}
