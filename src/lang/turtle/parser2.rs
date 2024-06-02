use chumsky::prelude::*;

use super::{
    token::{StringStyle, Token},
    Base, BlankNode, Literal, NamedNode, Prefix, RDFLiteral, Subject, Term, Triple, Turtle, PO,
};

use crate::model::{spanned, Spanned};

type S = std::ops::Range<usize>;
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

fn literal() -> impl Parser<Token, Literal, Error = Simple<Token>> + Clone {
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

fn named_node() -> impl Parser<Token, NamedNode, Error = Simple<Token>> + Clone {
    select! {
        Token::PredType => NamedNode::A,
        Token::IRIRef(x) => NamedNode::Full(x),
        Token::PNameLN(x, b) => NamedNode::Prefixed { prefix: x.unwrap_or_default(), value: b },
    }
}

fn expect_token(token: Token) -> impl Parser<Token, Token, Error = Simple<Token>> + Clone {
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
    // just(token.clone())
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

fn subject() -> impl Parser<Token, Subject, Error = Simple<Token>> + Clone {
    named_node()
        .map(|x| Subject::NamedNode(x))
        .or(blank_node().map(|x| Subject::BlankNode(x)))
}

fn term(
    bn: impl Clone + Parser<Token, BlankNode, Error = Simple<Token>> + 'static,
) -> impl Parser<Token, Term, Error = Simple<Token>> + Clone {
    recursive(|term| {
        let collection = term
            .map_with_span(spanned)
            .repeated()
            .delimited_by(just(Token::ColEnd), just(Token::ColStart))
            .map(|x| Term::Collection(x));

        let nn = named_node().map(|x| Term::NamedNode(x));
        let blank = bn.map(|x| Term::BlankNode(x));
        let literal = literal().map(|x| Term::Literal(x));

        collection.or(nn).or(blank).or(literal)
    })
}

fn po(
    bn: impl Clone + Parser<Token, BlankNode, Error = Simple<Token>> + 'static,
) -> impl Parser<Token, PO, Error = Simple<Token>> + Clone {
    term(bn)
        .map_with_span(spanned)
        .separated_by(just(Token::ObjectSplit))
        .map(|mut x| {
            x.reverse();
            x
        })
        // .allow_leading() // TODO check in grammar
        .then(
            named_node().map_with_span(spanned), //  HERE
        )
        .map(|(object, predicate)| PO { predicate, object })
}

fn triple() -> impl Parser<Token, Triple, Error = Simple<Token>> + Clone {
    let subj = subject().or(empty().validate(|_, span: S, emit| {
        emit(Simple::custom(
            span.end..span.end,
            format!("Expected a subject."),
        ));
        Subject::NamedNode(NamedNode::Invalid)
    }));
    // let subj = subject();

    expect_token(Token::Stop)
        .ignore_then(
            po(blank_node())
                .map_with_span(spanned)
                .separated_by(just(Token::PredicateSplit))
                .allow_leading()
                .map(|mut x| {
                    x.reverse();
                    x
                }),
        )
        .then(
            subj.map_with_span(spanned), // HERE
        )
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

#[allow(unused)]
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

            Turtle::new(base, prefixes, triples, location)
        })
        .then_ignore(end())
}

#[cfg(test)]
pub mod turtle_tests {
    use std::str::FromStr;

    use chumsky::{prelude::Simple, Parser, Stream};

    use crate::lang::turtle::{
        parser2::{blank_node, named_node, prefix, triple, turtle},
        token::Token,
        tokenizer, BlankNode,
    };

    use super::literal;

    pub fn parse_it<T, P: Parser<Token, T, Error = Simple<Token>>>(
        turtle: &str,
        parser: P,
    ) -> (Option<T>, Vec<Simple<Token>>) {
        let tokens = tokenizer::parse_tokens().parse(turtle).unwrap();
        let end = turtle.len()..turtle.len();
        let stream = Stream::from_iter(end, tokens.into_iter().rev().filter(|x| !x.0.is_comment()));

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
        let txt = "
<b> <c>.";
        let (output, errors) = parse_it(txt, turtle(&url));
        println!("Erorrs {:?}", errors);
        println!("Outpput {:?}", output);

        assert_eq!(errors.len(), 1);
        assert_eq!(
            output.unwrap().to_string(),
            "invalid <b> <c>.\n"
        );
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
