use chumsky::prelude::*;

use crate::model::{spanned, Spanned};

#[derive(Clone)]
enum LiteralHelper {
    LangTag(String),
    DataType(NamedNode),
    None,
}

impl LiteralHelper {
    fn to_lit(self, (value, quote_style): (String, StringStyle)) -> RDFLiteral {
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

    let dt = just(Token::DataTypeDelim)
        .ignore_then(named_node().or(empty().to(NamedNode::Invalid)))
        .map(|x| LiteralHelper::DataType(x));

    let rdf = select! {
        Token::Str(x, style) => (x, style)
    }
    .then(lt.or(dt).or(empty().to(LiteralHelper::None)))
    .map(|(x, h)| h.to_lit(x));

    rdf.map(|x| Literal::RDF(x)).or(select! {
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

fn blank_node() -> impl Parser<Token, BlankNode, Error = Simple<Token>> + Clone {
    recursive(|bn| {
        po(bn)
            .map_with_span(spanned)
            .separated_by(just(Token::PredicateSplit))
            .allow_trailing()
            .delimited_by(just(Token::BNodeStart), just(Token::BNodeEnd))
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
        term.map_with_span(spanned)
            .repeated()
            .delimited_by(just(Token::ColStart), just(Token::ColEnd))
            .map(|x| Term::Collection(x))
            .or(named_node()
                .map(|x| Term::NamedNode(x))
                .or(bn.map(|x| Term::BlankNode(x)))
                .or(literal().map(|x| Term::Literal(x))))
    })
}

fn po(
    bn: impl Clone + Parser<Token, BlankNode, Error = Simple<Token>> + 'static,
) -> impl Parser<Token, PO, Error = Simple<Token>> + Clone {
    named_node()
        .map_with_span(spanned)
        .then(
            term(bn)
                .map_with_span(spanned)
                .separated_by(just(Token::ObjectSplit)),
        )
        .map(|(predicate, object)| PO { predicate, object })
}

fn triple() -> impl Parser<Token, Triple, Error = Simple<Token>> + Clone {
    subject()
        .map_with_span(spanned)
        .then(
            po(blank_node())
                .map_with_span(spanned)
                .separated_by(just(Token::PredicateSplit))
                .allow_trailing(),
        )
        .then_ignore(just(Token::Stop))
        .map(|(subject, po)| Triple { subject, po })
}

fn base() -> impl Parser<Token, Base, Error = Simple<Token>> + Clone {
    just(Token::BaseTag)
        .map_with_span(|_, s| s)
        .then(named_node().map_with_span(spanned))
        .then_ignore(just(Token::Stop))
        .map(|(s, x)| Base(s, x))
}

fn prefix() -> impl Parser<Token, Prefix, Error = Simple<Token>> {
    just(Token::PrefixTag)
        .map_with_span(|_, s| s)
        .then(select! { |span| Token::PNameLN(x, _) => Spanned(x.unwrap_or_default(), span)})
        .then(named_node().map_with_span(spanned))
        .then_ignore(just(Token::Stop))
        .map(|((span, prefix), value)| Prefix {
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
    statement.repeated().map(|statements| {
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
}

#[cfg(test)]
pub mod turtle_tests {
    use chumsky::{prelude::Simple, Parser, Stream};

    use crate::lang::turtle::{
        parser::{blank_node, named_node, prefix, triple, turtle},
        token::Token,
        tokenizer, BlankNode,
    };

    use super::literal;

    #[derive(Debug)]
    pub enum Err {
        Tokenizing,
        Parsing,
    }

    pub fn parse_it<T, P: Parser<Token, T, Error = Simple<Token>>>(
        turtle: &str,
        parser: P,
    ) -> Result<T, Err> {
        let tokens = tokenizer::parse_tokens().parse(turtle).map_err(|err| {
            println!("Token error {:?}", err);
            Err::Tokenizing
        })?;
        let end = turtle.len() - 1..turtle.len() + 1;
        let stream = Stream::from_iter(end, tokens.into_iter().filter(|x| !x.0.is_comment()));

        parser.parse(stream).map_err(|err| {
            println!("Parse error {:?}", err);
            Err::Parsing
        })
    }

    #[test]
    fn parse_literal() {
        let turtle = "42";
        let output = parse_it(turtle, literal()).expect("number");
        assert_eq!(output.to_string(), "42");

        let turtle = "\"42\"@en";
        let output = parse_it(turtle, literal()).expect("lang string");
        assert_eq!(output.to_string(), turtle);

        let turtle = "\"42\"^^xsd:int";
        let output = parse_it(turtle, literal()).expect("double quotes");
        assert_eq!(output.to_string(), turtle);

        let turtle = "\'42\'";
        let output = parse_it(turtle, literal()).expect("single quotes");
        assert_eq!(output.to_string(), turtle);
        let turtle = "\"\"\"42\"\"\"";
        let output = parse_it(turtle, literal()).expect("long double quotes");
        assert_eq!(output.to_string(), turtle);

        let turtle = "\'\'\'42\'\'\'";
        let output = parse_it(turtle, literal()).expect("long single quotes");
        assert_eq!(output.to_string(), turtle);
    }

    #[test]
    fn parse_prefix() {
        let turtle = "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .";
        let output = parse_it(turtle, prefix()).expect("Simple");
        assert_eq!(
            output.to_string(),
            "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
        );
    }

    #[test]
    fn parse_namednode() {
        let turtle = "<abc>";
        let output = parse_it(turtle, named_node()).expect("Simple");
        assert_eq!(output.to_string(), "<abc>");

        let turtle = "a";
        let output = parse_it(turtle, named_node()).expect("a");
        assert_eq!(output.to_string(), "a");

        let turtle = ":";
        let output = parse_it(turtle, named_node()).expect(":");
        assert_eq!(output.to_string(), ":");

        let turtle = "foo:bar";
        let output = parse_it(turtle, named_node()).expect("foo:bar");
        assert_eq!(output.to_string(), "foo:bar");
    }

    #[test]
    fn parse_blanknode() {
        let turtle = "[]";
        let output = parse_it(turtle, blank_node()).expect("anon");
        let is_unamed = match output {
            BlankNode::Unnamed(_) => true,
            _ => false,
        };
        assert!(is_unamed);

        let turtle = "_:foobar";
        let output = parse_it(turtle, blank_node()).expect("other bn");
        let is_named = match output {
            BlankNode::Named(_) => true,
            _ => false,
        };
        assert!(is_named);
    }

    #[test]
    fn parse_triple() {
        let turtle = "<a> <b> <c> .";
        let output = parse_it(turtle, triple()).expect("simple");
        assert_eq!(output.to_string(), "<a> <b> <c>.");

        let turtle = "<a> <b> <c> , <d> .";
        let output = parse_it(turtle, triple()).expect("object list");
        assert_eq!(output.to_string(), "<a> <b> <c>, <d>.");

        let turtle = "[ <d> <e> ] <b> <c> .";
        let output = parse_it(turtle, triple()).expect("blank node list");
        assert_eq!(output.to_string(), "[ <d> <e> ; ] <b> <c>.");

        let turtle = "[ <d> <e> ; <f> <g> ;  ] <b> <c> .";
        let output = parse_it(turtle, triple()).expect("blank node po list");
        assert_eq!(output.to_string(), "[ <d> <e> ;<f> <g> ; ] <b> <c>.");

        let turtle = "<a> <b> [ ] .";
        let output = parse_it(turtle, triple()).expect("bnode object");
        assert_eq!(output.to_string(), "<a> <b> [ ].");
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
        let output = parse_it(txt, turtle("http://example.com/ns#")).expect("simple");
        assert_eq!(output.prefixes.len(), 1, "prefixes are parsed");
        assert_eq!(output.triples.len(), 1, "triples are parsed");
        assert!(output.base.is_some(), "base is parsed");
    }
}

use super::{
    token::{StringStyle, Token},
    Base, BlankNode, Literal, NamedNode, Prefix, RDFLiteral, Subject, Term, Triple, Turtle, PO,
};
