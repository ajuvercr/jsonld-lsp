use std::io::{self, Write};

use chumsky::{prelude::*, Parser, Stream};
use enum_methods::{EnumIntoGetters, EnumIsA, EnumToGetters};

use crate::model::{spanned, Spanned};

use super::tokenizer::JsonToken;

#[derive(Clone, PartialEq, Debug, EnumIntoGetters, EnumIsA, EnumToGetters)]
pub enum ObjectMember {
    Full(Spanned<JsonToken>, Spanned<Json>),
    Partial(
        Option<Spanned<JsonToken>>,
        Option<Spanned<()>>,
        Option<Spanned<Json>>,
    ),
}

#[derive(Clone, PartialEq, Debug, EnumIntoGetters, EnumIsA, EnumToGetters)]
pub enum Json {
    Invalid,
    Token(JsonToken),
    Array(Vec<Spanned<Json>>),
    Object(Vec<Spanned<ObjectMember>>),
}

pub struct JsonFormatter {
    pub indent: String,
    pub inc: usize,
}
impl JsonFormatter {
    pub fn inc(&mut self) {
        self.inc += 1;
    }

    pub fn decr(&mut self) {
        self.inc -= 1;
    }

    pub fn line(&mut self, writer: &mut impl Write) -> io::Result<()> {
        write!(writer, "\n")?;
        for _ in 0..self.inc {
            write!(writer, "{}", &self.indent)?;
        }
        Ok(())
    }

    pub fn format(&mut self, json: &Json, writer: &mut impl Write) -> io::Result<()> {
        use std::io::{Error, ErrorKind};
        match json {
            Json::Invalid => {
                return Result::Err(Error::new(ErrorKind::Other, "cannot format invalid json"))
            }
            Json::Token(t) => write!(writer, "{}", t)?,
            Json::Array(xs) => {
                write!(writer, "[")?;
                self.inc();
                self.line(writer)?;
                let mut first = true;
                for t in xs {
                    if !first {
                        write!(writer, ",")?;
                        self.line(writer)?;
                    }
                    self.format(&t.0, writer)?;
                    first = false;
                }
                self.decr();
                self.line(writer)?;
                write!(writer, "]")?;
            }
            Json::Object(xs) => {
                write!(writer, "{{")?;
                self.inc();
                self.line(writer)?;
                let mut first = true;
                for t in xs {
                    if !first {
                        write!(writer, ",")?;
                        self.line(writer)?;
                    }
                    match &t.0 {
                        ObjectMember::Full(x, y) => {
                            write!(writer, "{}: ", x.0)?;
                            self.format(y, writer)?;
                        }
                        ObjectMember::Partial(_, _, _) => {
                            return Result::Err(Error::new(
                                ErrorKind::Other,
                                "cannot format invalid json",
                            ))
                        }
                    }
                    first = false;
                }
                self.decr();
                self.line(writer)?;
                write!(writer, "}}")?;
            }
        }
        Ok(())
    }
}

pub struct JsonIter<'a> {
    stack: Vec<Result<&'a Spanned<Json>, Spanned<&'a JsonToken>>>,
}
impl<'a> Iterator for JsonIter<'a> {
    type Item = Spanned<&'a JsonToken>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let c = self.stack.pop()?;
            match c {
                Ok(s) => match &s.0 {
                    Json::Invalid => {}
                    Json::Token(token) => return Some(spanned(&token, s.1.clone())),
                    Json::Array(xs) => self.stack.extend(xs.iter().map(Result::Ok)),
                    Json::Object(xs) => {
                        for x in xs.iter() {
                            match x.0 {
                                ObjectMember::Full(ref x, ref s) => {
                                    self.stack.push(Err(x.as_ref()));
                                    self.stack.push(Ok(s));
                                }

                                ObjectMember::Partial(ref x, _, ref s) => {
                                    if let Some(x) = x {
                                        self.stack.push(Err(x.as_ref()));
                                    }
                                    if let Some(s) = s {
                                        self.stack.push(Ok(s));
                                    }
                                }
                            }
                        }
                    }
                },
                Err(x) => return Some(x),
            }
        }
    }
}
impl Spanned<Json> {
    pub fn iter<'a>(&'a self) -> JsonIter<'a> {
        JsonIter {
            stack: vec![Ok(self)],
        }
    }
}

impl Default for Json {
    fn default() -> Self {
        Self::Invalid
    }
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
            .map_with_span(spanned)
            .or_not()
            .then(just(Colon).to(()).map_with_span(spanned).or_not())
            .then(value.or_not().clone())
            .validate(|((s, p), o), span, emit| match (s, p, o) {
                (Some(s), Some(_), Some(o)) => ObjectMember::Full(s, o),
                (s, p, o) => {
                    emit(Simple::custom(span, "Erroneous object member"));
                    ObjectMember::Partial(s, p, o)
                }
            });

        let obj = just(CuOpen)
            .ignore_then(
                member.map_with_span(spanned).separated_by(just(Comma)), // .separated_by(just(Comma)),
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
        let (json, json_errors) = parse(source, tokens);

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
    #[ignore]
    fn parse_json_array_invalid() {
        let source = "[\"test\" :  , 42 ]";
        let (tokens, token_errors) = tokenize(source);
        let (json, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        // assert_eq!(json_errors.len(), 1);

        println!("Error: {:?}", json_errors);
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
        let (_, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        assert_eq!(json_errors.len(), 0);
    }
}
