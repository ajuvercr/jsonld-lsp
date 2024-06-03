use chumsky::{prelude::*, Span, Stream};
use tracing::info;

use super::{
    token::{StringStyle, Token},
    Base, BlankNode, Literal, NamedNode, Prefix, RDFLiteral, Term, Triple, Turtle, PO,
};

use crate::model::{spanned, Spanned};

type S = std::ops::Range<usize>;
// #[derive(Debug, Clone, Copy)]
// struct S {
//     len: usize,
//     start: usize,
//     end: usize,
// }
// impl Span for S {
//     type Context = usize;
//     type Offset = usize;
//
//     fn new(len: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
//         Self {
//             len,
//             start: range.start,
//             end: range.end,
//         }
//     }
//
//     fn context(&self) -> Self::Context {
//         self.len
//     }
//
//     fn start(&self) -> Self::Offset {
//         self.len - self.end
//     }
//
//     fn end(&self) -> Self::Offset {
//         self.len - self.start
//     }
// }

// type S = std::ops::Range<usize>;
#[derive(Clone)]
pub enum LiteralHelper {
    LangTag(String),
    DataType(NamedNode),
    None,
}

impl LiteralHelper {
    pub fn to_lit(self, (value, quote_style): (String, StringStyle)) -> RDFLiteral {
        match self {
            LiteralHelper::LangTag(lang) => RDFLiteral {
                value,
                quote_style,
                lang: Some(lang),
                ty: None,
            },
            LiteralHelper::DataType(dt) => RDFLiteral {
                value,
                quote_style,
                lang: None,
                ty: Some(dt),
            },
            LiteralHelper::None => RDFLiteral {
                value,
                quote_style,
                lang: None,
                ty: None,
            },
        }
    }
}

fn literal() -> impl Parser<Token, Literal, Error = Simple<Token, S>> + Clone {
    let lt = select! { Token::LangTag(x) => LiteralHelper::LangTag(x)};

    let dt = named_node()
        .then_ignore(just(Token::DataTypeDelim))
        .map(|x| LiteralHelper::DataType(x));

    lt.or(dt)
        .or(empty().to(LiteralHelper::None))
        .then(select! {
            Token::Str(x, style) => (x, style)
        })
        .map(|(h, x)| h.to_lit(x))
        .map(|x| Literal::RDF(x))
        .or(select! {
            Token::Number(x) => Literal::Numeric(x),
            Token::True => Literal::Boolean(true),
            Token::False => Literal::Boolean(false),
        })
}

fn named_node() -> impl Parser<Token, NamedNode, Error = Simple<Token, S>> + Clone {
    select! {
        Token::PredType => NamedNode::A,
        Token::IRIRef(x) => NamedNode::Full(x),
        Token::PNameLN(x, b) => NamedNode::Prefixed { prefix: x.unwrap_or_default(), value: b },
    }
}

fn expect_token(token: Token) -> impl Parser<Token, Token, Error = Simple<Token, S>> + Clone {
    just(token.clone()).or(none_of([token.clone()])
        .rewind()
        .validate(move |_, span: S, emit| {
            emit(Simple::expected_input_found(
                span,
                [Some(token.clone())],
                None,
            ));
            token.clone()
        }))
}

fn blank_node() -> impl Parser<Token, BlankNode, Error = Simple<Token>> + Clone {
    recursive(|bn| {
        po(bn)
            .map_with_span(spanned)
            .separated_by(just(Token::PredicateSplit))
            .allow_leading()
            .map(|mut x| {
                x.reverse();
                x
            })
            .delimited_by(just(Token::BNodeEnd), just(Token::BNodeStart))
            .map(|x| BlankNode::Unnamed(x))
            .or(select! {
                Token::BlankNodeLabel(x) => BlankNode::Named(x),
            })
    })
}

fn subject() -> impl Parser<Token, Term, Error = Simple<Token, S>> + Clone {
    named_node()
        .map(|x| Term::NamedNode(x))
        .or(blank_node().map(|x| Term::BlankNode(x)))
}

fn term(
    bn: impl Clone + Parser<Token, BlankNode, Error = Simple<Token>> + 'static,
) -> impl Parser<Token, Term, Error = Simple<Token>> + Clone {
    recursive(|term| {
        let collection = term
            .map_with_span(spanned)
            .repeated()
            .map(|mut x| {
                x.reverse();
                x
            })
            .delimited_by(just(Token::ColEnd), just(Token::ColStart))
            .map(|x| Term::Collection(x));

        let nn = named_node().map(|x| Term::NamedNode(x));
        let blank = bn.map(|x| Term::BlankNode(x));
        let literal = literal().map(|x| Term::Literal(x));

        collection.or(literal).or(nn).or(blank)
    })
}

fn po(
    bn: impl Clone + Parser<Token, BlankNode, Error = Simple<Token>> + 'static,
) -> impl Parser<Token, PO, Error = Simple<Token>> + Clone {
    term(bn.clone())
        .map_with_span(spanned)
        .separated_by(just(Token::ObjectSplit))
        .map(|mut x| {
            x.reverse();
            x
        })
        // .allow_leading() // TODO check in grammar
        .then_with(move |os| {
            let os1 = os.clone();
            let predicate = term(bn.clone())
                .map_with_span(spanned)
                .map(move |pred| (os1.clone(), pred));

            let os2 = os.clone();
            // let end = os[0].span().end;

            let alt_pred = just(Token::PredicateSplit)
                .rewind()
                .validate(move |_, span: S, emit| {
                    emit(Simple::custom(
                        span.end..span.end,
                        format!("Expected an object."),
                    ));
                    ()
                })
                .map(move |_| {
                    let mut os = os2.clone();
                    let pred = os[0].clone();
                    os[0] = spanned(Term::NamedNode(NamedNode::Invalid), os[0].span().clone());

                    (os, pred)
                });

            predicate.or(alt_pred)
        })
        .map(|(object, predicate)| PO { predicate, object })
}

fn triple() -> impl Parser<Token, Triple, Error = Simple<Token>> + Clone {
    expect_token(Token::Stop)
        .ignore_then(
            po(blank_node())
                .map_with_span(spanned)
                .separated_by(just(Token::PredicateSplit))
                .allow_leading()
                .at_least(1)
                .map(|mut x| {
                    x.reverse();
                    x
                }),
        )
        .then_with(move |po| {
            let po2 = po.clone();
            let basic_subj = subject()
                .map_with_span(spanned)
                .map(move |subj| (po2.clone(), subj));

            let end = po[0].span().end;
            let alt_subj = empty().validate(move |_, _: S, emit| {
                emit(Simple::custom(end..end, format!("Expected a predicate.")));

                let mut po = po.clone();
                let first = po[0].value_mut();

                let subj = first.predicate.clone();
                first.predicate = first.object.pop().unwrap();

                first.object.push(Spanned(
                    Term::NamedNode(NamedNode::Invalid),
                    first.predicate.span().clone(),
                ));

                // Subject::NamedNode(NamedNode::Invalid)
                (po, subj)
            });

            basic_subj.or(alt_subj)
        })
        .map(|(po, subject)| Triple { subject, po })
}

fn base() -> impl Parser<Token, Base, Error = Simple<Token>> + Clone {
    expect_token(Token::Stop)
        .ignore_then(named_node().map_with_span(spanned))
        .then(just(Token::BaseTag).map_with_span(|_, s| s))
        .map(|(x, s)| Base(s, x))
}

fn prefix() -> impl Parser<Token, Prefix, Error = Simple<Token>> {
    expect_token(Token::Stop)
        .ignore_then(named_node().map_with_span(spanned))
        .then(select! { |span| Token::PNameLN(x, _) => Spanned(x.unwrap_or_default(), span)})
        .then(just(Token::PrefixTag).map_with_span(|_, s| s))
        .map(|((value, prefix), span)| Prefix {
            span,
            prefix,
            value,
        })
}

// Makes it easier to handle parts that are not ordered
enum Statement {
    Base(Spanned<Base>),
    Prefix(Spanned<Prefix>),
    Triple(Spanned<Triple>),
}

pub fn turtle<'a>(
    location: &'a lsp_types::Url,
) -> impl Parser<Token, Turtle, Error = Simple<Token>> + 'a {
    let base = base().map_with_span(spanned).map(|b| Statement::Base(b));
    let prefix = prefix()
        .map_with_span(spanned)
        .map(|b| Statement::Prefix(b));
    let triple = triple()
        .map_with_span(spanned)
        .map(|b| Statement::Triple(b));

    let statement = base.or(prefix).or(triple);
    statement
        .repeated()
        .map(|statements| {
            let mut base = None;
            let mut prefixes = Vec::new();
            let mut triples = Vec::new();
            for statement in statements {
                match statement {
                    Statement::Base(b) => base = Some(b),
                    Statement::Prefix(p) => prefixes.push(p),
                    Statement::Triple(t) => triples.push(t),
                }
            }
            prefixes.reverse();
            triples.reverse();

            Turtle::new(base, prefixes, triples, location)
        })
        .then_ignore(end())
}

pub fn parse_turtle(
    location: &lsp_types::Url,
    tokens: Vec<Spanned<Token>>,
    len: usize,
) -> (Spanned<Turtle>, Vec<(usize, Simple<Token>)>) {
    let rev_range = |range: std::ops::Range<usize>| (len - range.end)..(len - range.start);
    let stream = chumsky::Stream::from_iter(
        0..len,
        tokens
            .into_iter()
            .rev()
            .filter(|x| !x.is_comment())
            .map(|Spanned(x, s)| (x, rev_range(s))),
    );

    let parser = turtle(location)
        .map_with_span(spanned)
        .then_ignore(end().recover_with(skip_then_retry_until([])));

    let (mut json, json_errors) = parser.parse_recovery(stream);

    json.iter_mut().for_each(|turtle| turtle.0.fix_spans(len));

    let json_errors: Vec<_> = json_errors.into_iter().map(|error| (len, error)).collect();
    if !json_errors.is_empty() {
        info!("Errors");
        for e in &json_errors {
            info!("Error {:?}", e);
        }
    }

    (
        json.unwrap_or(Spanned(Turtle::empty(location), 0..len)),
        json_errors,
    )
}

#[cfg(test)]
pub mod turtle_tests {
    use std::str::FromStr;

    use chumsky::{prelude::Simple, Parser, Stream};

    use crate::{
        lang::turtle::{
            parser2::{blank_node, named_node, prefix, triple, turtle},
            token::Token,
            tokenizer, BlankNode,
        },
        model::Spanned,
    };

    use super::literal;

    pub fn parse_it<T, P: Parser<Token, T, Error = Simple<Token>>>(
        turtle: &str,
        parser: P,
    ) -> (Option<T>, Vec<Simple<Token>>) {
        let tokens = tokenizer::parse_tokens().parse(turtle).unwrap();
        let end = turtle.len()..turtle.len();
        let stream = Stream::from_iter(
            end,
            tokens
                .into_iter()
                .map(|Spanned(x, y)| (x, y))
                .rev()
                .filter(|x| !x.0.is_comment()),
        );

        parser.parse_recovery(stream)
    }

    #[test]
    fn parse_literal() {
        let turtle = "42";
        let output = parse_it(turtle, literal()).0.expect("number");
        assert_eq!(output.to_string(), "42");

        let turtle = "\"42\"@en";
        let output = parse_it(turtle, literal()).0.expect("lang string");
        assert_eq!(output.to_string(), turtle);

        let turtle = "\"42\"^^xsd:int";
        let output = parse_it(turtle, literal()).0.expect("double quotes");
        assert_eq!(output.to_string(), turtle);

        let turtle = "\'42\'";
        let output = parse_it(turtle, literal()).0.expect("single quotes");
        assert_eq!(output.to_string(), turtle);
        let turtle = "\"\"\"42\"\"\"";
        let output = parse_it(turtle, literal()).0.expect("long double quotes");
        assert_eq!(output.to_string(), turtle);

        let turtle = "\'\'\'42\'\'\'";
        let output = parse_it(turtle, literal()).0.expect("long single quotes");
        assert_eq!(output.to_string(), turtle);
    }

    #[test]
    fn parse_prefix() {
        let turtle = "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .";
        let output = parse_it(turtle, prefix()).0.expect("Simple");
        assert_eq!(
            output.to_string(),
            "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
        );
    }

    #[test]
    fn parse_namednode() {
        let turtle = "<abc>";
        let output = parse_it(turtle, named_node()).0.expect("Simple");
        assert_eq!(output.to_string(), "<abc>");

        let turtle = "a";
        let output = parse_it(turtle, named_node()).0.expect("a");
        assert_eq!(output.to_string(), "a");

        let turtle = ":";
        let output = parse_it(turtle, named_node()).0.expect(":");
        assert_eq!(output.to_string(), ":");

        let turtle = "foo:bar";
        let output = parse_it(turtle, named_node()).0.expect("foo:bar");
        assert_eq!(output.to_string(), "foo:bar");
    }

    #[test]
    fn parse_blanknode() {
        let turtle = "[]";
        let output = parse_it(turtle, blank_node()).0.expect("anon");
        let is_unamed = match output {
            BlankNode::Unnamed(_) => true,
            _ => false,
        };
        assert!(is_unamed);

        let turtle = "_:foobar";
        let output = parse_it(turtle, blank_node()).0.expect("other bn");
        let is_named = match output {
            BlankNode::Named(_) => true,
            _ => false,
        };
        assert!(is_named);
    }

    #[test]
    fn parse_triple() {
        let turtle = "<a> <b> <c> .";
        let output = parse_it(turtle, triple()).0.expect("simple");
        assert_eq!(output.to_string(), "<a> <b> <c>.");

        let turtle = "<a> <b> <c> , <d> .";
        let output = parse_it(turtle, triple()).0.expect("object list");
        assert_eq!(output.to_string(), "<a> <b> <c>, <d>.");

        let turtle = "[ <d> <e> ] <b> <c> .";
        let output = parse_it(turtle, triple()).0.expect("blank node list");
        assert_eq!(output.to_string(), "[ <d> <e> ; ] <b> <c>.");

        let turtle = "[ <d> <e> ; <f> <g> ;  ] <b> <c> .";
        let output = parse_it(turtle, triple()).0.expect("blank node po list");
        assert_eq!(output.to_string(), "[ <d> <e> ;<f> <g> ; ] <b> <c>.");

        let turtle = "<a> <b> [ ] .";
        let output = parse_it(turtle, triple()).0.expect("bnode object");
        assert_eq!(output.to_string(), "<a> <b> [ ].");
    }

    #[test]
    fn parse_triple_with_recovery_no_end() {
        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let txt = "<a> <b> <c>";
        let (output, errors) = parse_it(txt, turtle(&url));

        assert_eq!(errors.len(), 1);
        assert_eq!(output.unwrap().to_string(), "<a> <b> <c>.\n");
    }

    #[test]
    fn parse_triple_with_recovery_no_object() {
        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let txt = "<b> <c> .";
        let (output, errors) = parse_it(txt, turtle(&url));

        println!("output {:?}", output);
        println!("errors {:?}", errors);

        assert_eq!(errors.len(), 1);
        assert_eq!(output.unwrap().to_string(), "<b> <c> invalid.\n");
    }

    #[test]
    fn parse_triple_with_recovery_unfinished_object() {
        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let txt = "<a> <b> <c>; <d> .";
        let (output, errors) = parse_it(txt, turtle(&url));

        println!("output {:?}", output);
        println!("errors {:?}", errors);

        assert_eq!(errors.len(), 1);
        assert_eq!(output.unwrap().to_string(), "<a> <b> <c>; <d> invalid.\n");
    }

    #[test]
    fn parse_turtle() {
        let txt = r#"
        @base <>. #This is a very nice comment!
#This is a very nice comment!
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
<a> <b> <c>.
#This is a very nice comment!
            "#;
        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let output = parse_it(txt, turtle(&url)).0.expect("simple");
        assert_eq!(output.prefixes.len(), 1, "prefixes are parsed");
        assert_eq!(output.triples.len(), 1, "triples are parsed");
        assert!(output.base.is_some(), "base is parsed");
    }
}
