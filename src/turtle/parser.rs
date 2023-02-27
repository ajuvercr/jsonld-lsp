use chumsky::{prelude::*, text::Character};

use crate::model::spanned;

use super::{BlankNode, Literal, NamedNode, Subject, Term, Triple, O, PO};

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
    just('<')
        .ignore_then(
            filter(|c| *c != '>' && *c != '\\' && !(*c as char).is_whitespace())
                .or(escape())
                .repeated(),
        )
        .then_ignore(just('>'))
        .collect::<String>()
        .map(|x| NamedNode(x))
        .labelled("NamedNode")
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

fn parse_blanknode() -> impl Parser<char, BlankNode, Error = Simple<char>> {
    just('_')
        .then(just(':'))
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape()).repeated())
        .collect::<String>()
        .map(|x| BlankNode(x))
        .labelled("blank node")
}

fn parse_term() -> impl Parser<char, Term, Error = Simple<char>> {
    parse_literal()
        .map(|x| Term::Literal(x))
        .or(parse_namednode().map(|x| Term::NamedNode(x)))
        .or(parse_blanknode().map(|b| Term::BlankNode(b)))
}

fn parse_predicate_object() -> impl Parser<char, PO, Error = Simple<char>> {
    recursive(|ppo| {
        let po = parse_object(ppo.clone()).map_with_span(spanned).padded();
        let po2 = parse_object(ppo).map_with_span(spanned).padded();
        parse_namednode()
            .map_with_span(spanned)
            .padded()
            .then(po.chain(just(',').ignore_then(po2).repeated()))
            .map(|(predicate, object)| PO {
                predicate,
                object,
            })
    })
}

fn parse_object(
    ppo: impl Parser<char, PO, Error = Simple<char>> + Clone,
) -> impl Parser<char, O, Error = Simple<char>> {
    let ppo2 = ppo.clone().map_with_span(spanned).padded();
    let ppo = ppo.map_with_span(spanned).padded();
    parse_term()
        .map_with_span(spanned)
        .map(|x| O::Term(x))
        .or(ppo.chain(just(';').ignore_then(ppo2).repeated())
            .delimited_by(just('['), just(']')).padded()
            .map(|x| O::Object(x)))
}

pub fn parse_triple() -> impl Parser<char, Triple, Error = Simple<char>> {
    let subject = parse_blanknode()
        .map(|b| Subject::BlankNode(b))
        .or(parse_namednode().map(|n| Subject::NamedNode(n)))
        .map_with_span(spanned);

    let ppo = parse_predicate_object().map_with_span(spanned).padded();
    let ppo2 = parse_predicate_object().map_with_span(spanned).padded();

    subject
        .padded()
        .then(
            ppo.chain(just(';').ignore_then(ppo2).repeated())
        )
        .then_ignore(just('.'))
        .map(|(subject, po)| Triple { subject, po })
}

#[cfg(test)]
mod tests {
    use crate::turtle::{
        parser::{parse_blanknode, parse_literal},
        BlankNode,
    };
    use chumsky::Parser;

    use super::{parse_namednode, parse_triple};

    #[test]
    fn test_parse_namednode() {
        let res = parse_namednode().parse("<test>");
        assert!(res.is_ok());
        let res = parse_namednode().parse("<test");
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_blanknode() {
        let res = parse_blanknode().parse("_:b1");
        assert_eq!(res, Ok(BlankNode("b1".to_string())));
        let res = parse_blanknode().parse("__:b1");
        assert!(res.is_err());
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
            .and_then(|x| x.ty).map(|x| x.0);
        assert_eq!(ty, Some("typestring".to_string()));
    }

    #[test]
    fn test_parse_triple() {
        let res = parse_triple().parse("<hallo> <daar> \"literal\".");
        assert!(res.is_ok());

        let res = parse_triple().parse("<hallo> <daar> \"literal\", \"literal2\".");
        assert!(res.is_ok());

        let res = parse_triple().parse("<hallo> <daar> \"literal\", \"literal2\"; <daar2> <hier>.");
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_triple_bn() {
        let res = parse_triple().parse("<hallo> <intermediate> [ <daar> <hier> ].");

        assert!(res.is_ok());
        if let Ok(x) = res {
            println!("{}", x);
        }

        let res = parse_triple().parse("<hallo> <intermediate> [ <daar> <hier>; <daar2> <hier2> ].");
        assert!(res.is_ok());
        if let Ok(x) = res {
            println!("{}", x);
        }

        assert!(false);
    }
}
