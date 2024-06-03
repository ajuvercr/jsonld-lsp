use crate::model::spanned;
use crate::model::Spanned;

use super::token::{StringStyle, Token};
use chumsky::chain::Chain;
use chumsky::prelude::*;
use chumsky::Parser;

macro_rules! t {
    ($t:ty) => {
        impl Parser<char, $t, Error = Simple<char>>
    };
}

fn tok(st: &'static str, tok: Token) -> t!(Token) {
    just::<char, &str, Simple<char>>(st).to(tok)
}

fn keywords() -> t!(Token) {
    just('@').ignore_then(choice((
        just("prefix").to(Token::PrefixTag),
        just("base").to(Token::BaseTag),
    )))
}

fn tokens() -> t!(Token) {
    choice((
        // tok("@prefix", Token::PrefixTag),
        // tok("@base", Token::BaseTag),
        tok("PREFIX", Token::SparqlPrefix),
        tok("BASE", Token::SparqlBase),
        tok("[", Token::BNodeStart),
        tok("]", Token::BNodeEnd),
        tok("(", Token::ColStart),
        tok(")", Token::ColEnd),
        tok("^^", Token::DataTypeDelim),
        tok(".", Token::Stop),
        tok(",", Token::ObjectSplit),
        tok(";", Token::PredicateSplit),
        tok("a", Token::PredType),
        tok("true", Token::True),
        tok("false", Token::False),
    ))
}

fn comment() -> t!(Token) {
    just('#')
        .ignore_then(none_of("\n\r").repeated().collect())
        .map(|x| Token::Comment(x))
}

fn invalid() -> t!(Token) {
    none_of(" \n\r")
        .repeated()
        .at_least(1)
        .collect()
        .map(Token::Invalid)
}

fn iri_ref() -> t!(Token) {
    let letter = none_of("<>\"{}|^`\\").repeated().at_least(1).or(uchar());

    letter
        .repeated()
        .flatten()
        .collect()
        .delimited_by(just('<'), just('>'))
        .map(|x| Token::IRIRef(x))
}

fn pname_ns() -> t!(Token) {
    pn_prefix()
        .collect()
        .or_not()
        .then_ignore(just(':'))
        .then(pn_local().collect().or_not())
        .map(|(x, local)| {
            if let Some(local) = local {
                Token::PNameLN(x, local)
            } else {
                Token::PNameLN(x, String::new())
            }
        })
}

fn label_post() -> t!(Vec<char>) {
    just('.')
        .repeated()
        .chain(pn_chars().repeated().at_least(1))
}

fn blank_node_label() -> t!(Token) {
    let label = pn_chars()
        .or(filter(|c: &char| c.is_numeric()))
        .repeated()
        .then(label_post().repeated().flatten())
        .map(|(mut x, y)| {
            x.extend(y);
            x
        });

    just('_')
        .then(just(':'))
        .ignore_then(label.collect())
        .map(|x| Token::BlankNodeLabel(x))
}

fn lang_tag() -> t!(Token) {
    let rep = just('-').chain(filter(|c: &char| c.is_alphanumeric()).repeated());
    just('@')
        .ignore_then(filter(|c: &char| c.is_alphabetic()).repeated())
        .then(rep.repeated().flatten())
        .map(|(mut x, y)| {
            y.append_to(&mut x);
            x
        })
        .collect()
        .map(|string| Token::LangTag(string))
}

fn integer() -> t!(Token) {
    let before_dot = || {
        one_of("+-")
            .or_not()
            .then(filter(|c: &char| c.is_numeric()).repeated().at_least(1))
            .map(|(x, y)| {
                let mut o: Vec<char> = Vec::with_capacity(x.is_some() as usize + y.len());
                x.append_to(&mut o);
                y.append_to(&mut o);
                o
            })
    };

    let no_dot = || {
        filter(|c: &char| c.is_numeric())
            .repeated()
            .at_least(1)
            .then(exponent())
            .map(|(mut x, y)| {
                y.append_to(&mut x);
                x
            })
    };

    let with_dot = || {
        just('.').then(no_dot()).map(|(x, y)| {
            let mut o = Vec::with_capacity(1 + y.len());
            o.push(x);
            y.append_to(&mut o);
            o
        })
    };

    with_dot()
        .or(before_dot().then(with_dot()).map(|(mut x, y)| {
            y.append_to(&mut x);
            x
        }))
        .or(no_dot())
        .or(before_dot())
        .collect()
        .map(|x| Token::Number(x))
}

fn exponent() -> t!(Vec<char>) {
    one_of("eE")
        .then(one_of("+-").or_not())
        .then(filter(|c: &char| c.is_numeric()).repeated().at_least(1))
        .map(|((x, y), z)| {
            let mut o = Vec::with_capacity(1 + y.is_some() as usize + z.len());
            o.push(x);
            y.append_to(&mut o);
            z.append_to(&mut o);
            o
        })
}

fn parse_string<const C: char>() -> t!(String) {
    let letter = e_char().or(uchar()).or(filter(|c: &char| {
        *c != '\\' && *c != '\n' && *c != '\r' && *c != C
    })
    .repeated()
    .at_least(1));

    letter
        .repeated()
        .flatten()
        .collect()
        .delimited_by(just(C), just(C))
}

fn parse_long_string<const C: char>() -> t!(String) {
    let si = || just::<char, char, Simple<char>>(C);
    let delim = si().ignore_then(si()).ignore_then(si());

    let letter = e_char()
        .or(uchar())
        .or(filter(|c: &char| *c != C && *c != '\\')
            .repeated()
            .at_least(1));

    delim
        .ignore_then(
            si().repeated()
                .at_most(2)
                .then(letter.repeated().flatten())
                .map(|(mut x, y)| {
                    y.append_to(&mut x);
                    x
                }),
        )
        .then_ignore(delim)
        .collect()
}

fn strings() -> t!(Token) {
    long_string_double()
        .or(long_string_single())
        .or(string_single())
        .or(string_double())
}

fn string_single() -> t!(Token) {
    parse_string::<'\''>().map(|x| Token::Str(x, StringStyle::Single))
}
fn string_double() -> t!(Token) {
    parse_string::<'"'>().map(|x| Token::Str(x, StringStyle::Double))
}

fn long_string_single() -> t!(Token) {
    parse_long_string::<'\''>().map(|x| Token::Str(x, StringStyle::SingleLong))
}

fn long_string_double() -> t!(Token) {
    parse_long_string::<'"'>().map(|x| Token::Str(x, StringStyle::DoubleLong))
}

fn uchar() -> t!(Vec<char>) {
    let small = just('\\')
        .chain(just('u'))
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex());

    let big = just('\\')
        .chain(just('U'))
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex());

    small.or(big)
}

fn e_char() -> t!(Vec<char>) {
    just('\\')
        .then(one_of("tbnrf\"'\\"))
        .map(|(x, y)| vec![x, y])
}

fn pn_chars_base() -> t!(char) {
    filter(|c: &char| c.is_alphabetic())
}

fn pn_chars_u() -> t!(char) {
    pn_chars_base().or(just('_'))
}
fn pn_chars() -> t!(char) {
    pn_chars_u()
        .or(just('-'))
        .or(filter(|c: &char| c.is_numeric()))
}
fn pn_prefix() -> t!(Vec<char>) {
    let ne = just('.')
        .repeated()
        .then(pn_chars().repeated().at_least(1))
        .map(|(x, y)| {
            let mut o: Vec<char> = Vec::with_capacity(x.len() + y.len());
            x.append_to(&mut o);
            y.append_to(&mut o);
            o
        })
        .repeated()
        .flatten();

    pn_chars_base().then(ne.or_not()).map(|(x, y)| {
        if let Some(y) = y {
            let mut o = Vec::with_capacity(y.len() + 1);
            o.push(x);
            o.extend(y);
            o
        } else {
            vec![x]
        }
    })
}

fn pn_local() -> t!(Vec<char>) {
    let first_char = pn_chars_u()
        .or(filter(|c: &char| *c == ':' || c.is_numeric()))
        .repeated()
        .at_least(1)
        .or(plx());

    let other = || pn_chars().or(just(':')).or(just('%'));

    let rest = just('.')
        .repeated()
        .then(other().repeated().at_least(1))
        .map(|(x, y)| {
            let mut o: Vec<char> = Vec::with_capacity(x.len() + y.len());
            x.append_to(&mut o);
            y.append_to(&mut o);
            o
        })
        .repeated()
        .flatten();

    first_char.then(rest.or_not()).map(|(mut x, y)| {
        if let Some(y) = y {
            y.append_to(&mut x);
        }
        x
    })
}

fn plx() -> t!(Vec<char>) {
    percent().or(pn_local_esc())
}

fn percent() -> t!(Vec<char>) {
    just('%')
        .ignore_then(hex().then(hex()))
        .map(|(x, y)| vec![x, y])
}

fn hex() -> t!(char) {
    filter(|c: &char| c.is_ascii_hexdigit())
}

fn pn_local_esc() -> t!(Vec<char>) {
    just('\\')
        .then(one_of("_~.-!$&'()*+,;=/?#@%"))
        .map(|(x, y)| vec![x, y])
}

pub fn parse_token() -> t!(Token) {
    choice((
        keywords(),
        comment(),
        iri_ref(),
        pname_ns(),
        blank_node_label(),
        lang_tag(),
        integer(),
        strings(),
        tokens(),
    ))
    .recover_with(skip_parser(invalid()))
}

pub fn parse_tokens() -> t!(Vec<Spanned<Token>>) {
    parse_token()
        .map_with_span(spanned)
        .padded()
        .repeated()
        .then_ignore(end().recover_with(skip_then_retry_until([])))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(unused)]
    fn log_parse_results(tokens: Vec<(Token, Range<usize>)>, err: &Vec<Simple<char>>) {
        tokens.iter().for_each(|tk| {
            println!("{:?}", tk);
        });

        err.iter().for_each(|er| eprintln!("{:?}", er));
    }

    #[test]
    fn parse_keywords() {
        assert!(keywords().parse("@prefix").is_ok());
        assert!(tokens().parse(".").is_ok());
        assert!(iri_ref().parse("<testing>").is_ok());
        assert!(pname_ns().parse(":").is_ok());
        assert!(pname_ns().parse("testing:").is_ok());
        assert!(pname_ns().parse("testing:test").is_ok());
        assert!(blank_node_label().parse("_:test").is_ok());
        assert!(lang_tag().parse("@en").is_ok());
        assert!(integer().parse("14").is_ok());
        assert!(integer().parse("14.0").is_ok());
        assert!(strings().parse("'testing'").is_ok());
        assert!(strings().parse("\"testing\"").is_ok());
        assert!(comment().parse("# This is a nice comment").is_ok());

        assert!(parse_token().parse(".").is_ok());
    }

    #[test]
    fn parse_multiple_kws() {
        assert!(tokens()
            .padded()
            .repeated()
            .parse("@prefix @base . .")
            .is_ok());
        assert!(iri_ref()
            .padded()
            .repeated()
            .parse("<testing> <testing>")
            .is_ok());
        assert!(pname_ns()
            .padded()
            .repeated()
            .parse(": testing: testing:test")
            .is_ok());
        assert!(blank_node_label()
            .padded()
            .repeated()
            .parse("_:b1 _:b0")
            .is_ok());
        assert!(lang_tag().padded().repeated().parse("@en @en-nl").is_ok());
        assert!(integer().padded().repeated().parse("14 14").is_ok());
        assert!(strings()
            .padded()
            .repeated()
            .parse("\"testing\" 'testing'")
            .is_ok());
    }

    #[test]
    fn parse_directives() {
        let input = "
            @prefix elm: <http://elm.com/types#> .
            @prefix : <http://elm.com/types#> .
            @base <http://example.com/#> . 
            # Test comment!
        ";

        assert!(parse_tokens().parse(input).is_ok());
    }

    #[test]
    fn parse_named_node() {
        let input = "
            <http://localhost/elmBeta> 
            elm:Beta

            :testing
            ";

        let (tok, err) = parse_tokens().parse_recovery(input);
        assert!(tok.is_some());
        assert!(err.is_empty());
    }

    #[test]
    fn simple_test() {
        let input = "
            @prefix elm: <http://elm.com/types#> .
            @base <http://example.com/#> . 
            
            elm:Beta foaf:string \"cookie\"@en ;
                     foaf:astring \"jar\"^^xsd:string .

            elm:Bnode a [ foaf:name \"Kachan\" ; 
                          foaf:lastName \"Bruh\" ; 
                          foaf:email \"kb@kbc.be\", \"notkb@notkbc.be\" ].
            ";

        let (tok, err) = parse_tokens().parse_recovery(input);
        assert!(tok.is_some());
        assert!(err.is_empty());
    }

    #[test]
    fn complex_test() {
        let input = "
            @prefix rr: <http://www.w3.org/ns/r2rml#> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            @prefix ex: <http://example.com/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
            @prefix rml: <http://semweb.mmlab.be/ns/rml#> .
            @prefix ql: <http://semweb.mmlab.be/ns/ql#> .

            @base <http://example.com/base/> .

            <TriplesMap1>
              a rr:TriplesMap;
                    
              rml:logicalSource [ 
                rml:source \"student.csv\";
                rml:referenceFormulation ql:CSV
              ] ;
                
              rr:subjectMap [ 
                rr:template \"http://example.com/{Name}\" 
              ]; 
                
              rr:predicateObjectMap [ 
                rr:predicate foaf:name ; 
                rr:objectMap [ 
                  rml:reference \"Name\" 
                ]
              ].
            ";

        let (tok, err) = parse_tokens().parse_recovery(input);
        assert!(tok.is_some());
        assert!(err.is_empty());
    }

    #[test]
    fn parse_invalid() {
        let input = "
            @prefix elm: http .
            ";

        let (tok, err) = parse_tokens().parse_recovery(input);
        assert!(tok.is_some());

        println!("tokens {:?}", tok);
        assert_eq!(tok.unwrap().len(), 4);
        assert_eq!(err.len(), 1);
    }
}
