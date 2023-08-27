use chumsky::{prelude::*, text::Character};

use crate::{
    model::{spanned, Spanned},
    Error,
};

use super::{Base, BlankNode, Literal, NamedNode, Prefix, Subject, Term, Triple, Turtle, PO};

fn escape() -> impl Parser<char, char, Error = Simple<char>> {
    just('\\').ignore_then(
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
    )
}

fn parse_namednode() -> impl Parser<char, NamedNode, Error = Simple<char>> {
    let a = just('a').map(|_| NamedNode::A);

    let full = just('<')
        .ignore_then(
            filter(|c| *c != '>' && *c != '\\' && !(*c as char).is_whitespace())
                .or(escape())
                .repeated()
                .collect::<String>()
                .then_ignore(just('>'))
                .map(|x| NamedNode::Full(x))
                .recover_with(skip_then_retry_until(['>']).consume_end()),
        )
        .recover_with(nested_delimiters('<', '>', [], |_| NamedNode::Invalid))
        .labelled("NamedNode full");

    let simple_string = || {
        filter(|c| {
            let c = *c as char;
            c.is_ascii_alphanumeric() || c == '_' || c == '-'
        })
            .or(escape())
            .repeated()
            .collect::<String>()
    };

    let prefixed = simple_string()
        .then_ignore(just(':'))
        .then(simple_string())
        .padded()
        .map(|(p, n)| NamedNode::Prefixed {
            prefix: p,
            value: n,
        });

    a.or(full).or(prefixed)
}

fn parse_literal() -> impl Parser<char, Literal, Error = Simple<char>> {
    let string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape()).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .labelled("string");

    let lang_string = just('@')
        .ignore_then(filter(|c| !(*c as char).is_whitespace()).repeated())
        .collect::<String>()
        .labelled("lang_string");
    let ty_string = just('^')
        .then(just('^'))
        .ignore_then(parse_namednode())
        .labelled("type string");

    string
        .then(lang_string.or_not())
        .then(ty_string.or_not())
        .map(|((s, l), ty)| Literal {
            value: s,
            lang: l,
            ty,
        })
}

fn parse_blanknode<P: Parser<char, PO, Error = Simple<char>>>(
    po_parser: impl Fn() -> P,
) -> impl Parser<char, BlankNode, Error = Simple<char>> {
    let named = just('_')
        .then(just(':'))
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape()).repeated())
        .collect::<String>()
        .map(|x| BlankNode::Named(x))
        .labelled("named blank node");

    let unnamed = po_parser()
        .map_with_span(spanned)
        .padded()
        .separated_by(just(';'))
        .allow_trailing()
        .padded()
        .delimited_by(just('['), just(']'))
        .map(|x| BlankNode::Unnamed(x))
        .recover_with(nested_delimiters('[', ']', [('<', '>')], |_| {
            BlankNode::Invalid
        }))
        .recover_with(skip_then_retry_until([']']).consume_end())
        .labelled("unnamed blank node");

    unnamed.or(named)
}

fn parse_term<P: Parser<char, PO, Error = Simple<char>>>(
    f: impl Fn() -> P,
) -> impl Parser<char, Term, Error = Simple<char>> {
    parse_literal()
        .map(|x| Term::Literal(x))
        .or(parse_namednode().map(|x| Term::NamedNode(x)))
        .or(parse_blanknode(f).map(|b| Term::BlankNode(b)))
}

fn parse_predicate_object() -> impl Parser<char, PO, Error = Simple<char>> {
    recursive(|ppo| {
        // Mjam!
        let po = move || {
            let ppo = ppo.clone();
            parse_term(move || ppo.clone())
                .map_with_span(spanned)
                .padded()
        };

        parse_namednode()
            .map_with_span(spanned)
            .padded()
            .then(po().separated_by(just(',')).validate(|x, span, emit| {
                if x.is_empty() {
                    emit(Simple::custom(span, "Expected at least one term"));
                }
                x
            }))
            .map(|(predicate, object)| PO { predicate, object })
    })
}

pub fn parse_triple() -> impl Parser<char, Triple, Error = Simple<char>> {
    let subject = parse_blanknode(parse_predicate_object)
        .map(|b| Subject::BlankNode(b))
        .or(parse_namednode().map(|n| Subject::NamedNode(n)))
        .map_with_span(spanned);

    let ppo = || parse_predicate_object().map_with_span(spanned).padded();

    subject
        .padded()
        .then(ppo().separated_by(just(';')).allow_trailing())
        .then_ignore(just('.'))
        .map(|(subject, po)| Triple { subject, po })
}

pub fn parse_base() -> impl Parser<char, Base, Error = Simple<char>> {
    just("@base")
        .ignore_then(parse_namednode().map_with_span(spanned).padded())
        .then_ignore(just('.'))
        .recover_with(skip_then_retry_until(['.']))
        .map(|b| Base(b))
}

pub fn parse_prefix() -> impl Parser<char, Prefix, Error = Simple<char>> {
    let string = filter(|c| !(*c as char).is_whitespace() && *c != ':' && *c != '\\')
        .or(escape())
        .repeated()
        .collect::<String>()
        .map_with_span(spanned)
        .padded()
        .labelled("string");

    just("@prefix")
        .ignore_then(string)
        .then_ignore(just(":"))
        .then(parse_namednode().map_with_span(spanned).padded())
        .then_ignore(just('.'))
        .map(|(p, c)| Prefix {
            prefix: p,
            value: c,
        })
}

pub fn parse_turtle() -> impl Parser<char, Turtle, Error = Simple<char>> {
    parse_base()
        .map_with_span(spanned)
        .padded()
        .or_not()
        .then(
            parse_prefix()
                .map_with_span(spanned)
                .padded()
                .recover_with(skip_then_retry_until(['.']))
                .repeated(),
        )
        .then(
            parse_triple()
                .map_with_span(spanned)
                .padded()
                .recover_with(skip_then_retry_until(['.']))
                .repeated(),
        )
        .map(|((base, prefixes), triples)| Turtle {
            base,
            prefixes,
            triples,
        })
        .then_ignore(end().recover_with(skip_then_retry_until([])))
        .padded()
}

pub fn parse(source: &str) -> (Turtle, Vec<Error>) {
    let (turtle, errs) = parse_turtle().parse_recovery(source);
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

    let turtle = turtle.unwrap_or_default();
    (turtle, errors)
}

#[cfg(test)]
mod tests {
    use crate::turtle::{
        parse_prefix,
        parser::{parse_blanknode, parse_literal, parse_predicate_object},
        BlankNode, NamedNode,
    };
    use chumsky::Parser;

    use super::{parse_base, parse_namednode, parse_triple, parse_turtle};

    #[test]
    fn test_parse_namednode() {
        let res = parse_namednode().parse("<test>");
        assert!(res.is_ok());
        let res = parse_namednode().parse("<test");
        assert!(res.is_err());

        let res = parse_namednode().parse("foaf:name");
        assert_eq!(
            res,
            Ok(NamedNode::Prefixed {
                prefix: "foaf".to_string(),
                value: "name".to_string()
            })
        );
    }

    #[test]
    fn test_parse_blanknode() {
        let res = parse_blanknode(parse_predicate_object).parse("_:b1");
        assert_eq!(res, Ok(BlankNode::Named("b1".to_string())));
        let res = parse_blanknode(parse_predicate_object).parse("__:b1");
        assert!(res.is_err());

        let res = parse_blanknode(parse_predicate_object).parse("[]");
        assert!(res.is_ok());

        let (res, errors) = parse_blanknode(parse_predicate_object).parse_recovery("[ <abc> ]");
        println!("{:?} {:?}", res, errors);
        assert!(res.is_some());
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_parse_literal() {
        let res = parse_literal().parse("\"<test>\"");
        assert!(res.is_ok());
        let res = parse_literal().parse("\"<test>");
        assert!(res.is_err());

        let lang = parse_literal()
            .parse("\"test\"@en")
            .ok()
            .and_then(|x| x.lang);
        assert_eq!(lang, Some("en".to_string()));

        let ty = parse_literal()
            .parse("\"test\"^^<typestring>")
            .ok()
            .and_then(|x| x.ty)
            .map(|x| x);
        assert_eq!(ty, Some(NamedNode::Full("typestring".to_string())));
    }

    #[test]
    fn test_parse_triple() {
        let res = parse_triple().parse("<hallo> <daar> \"literal\".");
        assert!(res.is_ok());

        let res = parse_triple().parse("<hallo> <daar> \"literal\", \"literal2\".");
        assert!(res.is_ok());

        let res = parse_triple().parse("<hallo> <daar> \"literal\", \"literal2\"; <daar2> <hier>.");
        assert!(res.is_ok());

        let triple = r#"
<#green-goblin>
    rel:enemyOf <#spiderman> ;
    a foaf:Person ;   
    foaf:name "Green Goblin" .
        "#;
        let res = parse_triple().parse(triple);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_triple_bn() {
        let res = parse_triple().parse("<hallo> <intermediate> [ <daar> <hier> ].");
        assert!(res.is_ok());

        let res = parse_triple().parse("<hallo> <intermediate> [ <daar> <hier> ; ].");
        assert!(res.is_ok());

        let res = parse_triple().parse("[ foaf:name \"arthur\" , [ foaf:like \"Vercruysse\" ]; ] <intermediate> [ <daar> <hier> ; ].");
        println!("{:?}", res);
        assert!(res.is_ok());

        let res =
            parse_triple().parse("<hallo> <intermediate> [ <daar> <hier>; foaf:here <hier2> ].");
        assert!(res.is_ok());

        let res =
            parse_triple().parse("<hallo> <intermediate> [ <daar> <hier>; foaf:here <hier2>.");
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_base() {
        let res = parse_base().parse("@base <test>.");
        assert!(res.is_ok());

        let res = parse_base().parse("@base <test>");
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_prefix() {
        let res = parse_prefix().parse("@prefix foaf: <test>.");
        assert!(res.is_ok());

        let res = res.unwrap();
        assert_eq!(res.prefix.value(), "foaf");

        let res = parse_prefix().parse("@prefix foaf: <test>");
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_turtle() {
        let turtle = r#"
@base <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rel: <http://www.perceive.net/schemas/relationship/> .

<#green-goblin>
    rel:enemyOf <#spiderman> ;
    a foaf:Person ;   
    foaf:name "Green Goblin" .

<#spiderman>
    rel:enemyOf <#green-goblin> ;
    a foaf:Person ;
    foaf:name "Spiderman", "Человек-паук"@ru .
            "#;

        let res = parse_turtle().parse(turtle);
        assert!(res.is_ok());

        let res = res.unwrap();
        println!("{}", res);
        assert_eq!(res.triples.len(), 2);
        assert_eq!(res.prefixes.len(), 4);
    }
}
