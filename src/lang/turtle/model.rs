use hashbrown::HashSet;
use sophia_api::term::GraphName;
use sophia_iri::resolve::{BaseIri, IriParseError};

use super::{shacl::MyTerm, token::StringStyle};
use crate::model::Spanned;
use std::{fmt::Display, ops::Range};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    RDF(RDFLiteral),
    Boolean(bool),
    Numeric(String),
}

impl Literal {
    pub fn plain_string(&self) -> String {
        match self {
            Literal::RDF(s) => s.plain_string(),
            Literal::Boolean(x) => x.to_string(),
            Literal::Numeric(x) => x.clone(),
        }
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::RDF(x) => x.fmt(f),
            Literal::Boolean(x) => write!(f, "{}", x),
            Literal::Numeric(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RDFLiteral {
    pub value: String,
    pub quote_style: StringStyle,
    pub lang: Option<String>,
    pub ty: Option<NamedNode>,
}

impl RDFLiteral {
    pub fn plain_string(&self) -> String {
        self.value.to_string()
    }
}

impl Display for RDFLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let quote = match self.quote_style {
            StringStyle::DoubleLong => "\"\"\"",
            StringStyle::Double => "\"",
            StringStyle::SingleLong => "'''",
            StringStyle::Single => "'",
        };
        match (&self.lang, &self.ty) {
            (None, None) => write!(f, "{}{}{}", quote, self.value, quote),
            (None, Some(t)) => write!(f, "{}{}{}^^{}", quote, self.value, quote, t),
            (Some(l), None) => write!(f, "{}{}{}@{}", quote, self.value, quote, l),
            (Some(l), Some(t)) => write!(f, "{}{}{}@{}^^{}", quote, self.value, quote, l, t),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NamedNode {
    Full(String),
    Prefixed { prefix: String, value: String },
    A,
    Invalid,
}

impl NamedNode {
    pub fn expand(&self, turtle: &Turtle) -> Option<String> {
        let base = turtle.get_base();
        let out = self.expand_step(turtle, HashSet::new())?;

        let url = base.join(&out).ok()?;

        Some(url.to_string())
    }

    pub fn expand_step<'a>(
        &'a self,
        turtle: &Turtle,
        mut done: HashSet<&'a str>,
    ) -> Option<String> {
        match self {
            Self::Full(s) => s.clone().into(),
            Self::Prefixed { prefix, value } => {
                if done.contains(prefix.as_str()) {
                    return None;
                }
                done.insert(prefix);
                let prefix = turtle
                    .prefixes
                    .iter()
                    .find(|x| x.0.prefix.as_str() == prefix.as_str())?;

                let expaned = prefix.value.expand_step(turtle, done)?;
                Some(format!("{}{}", expaned, value))
            }
            Self::A => Some("http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string()),
            Self::Invalid => None,
        }
    }
}

impl Display for NamedNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamedNode::Full(x) => write!(f, "<{}>", x),
            NamedNode::Prefixed { prefix, value } => write!(f, "{}:{}", prefix, value),
            NamedNode::A => write!(f, "a"),
            NamedNode::Invalid => write!(f, "invalid"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlankNode {
    Named(String),
    Unnamed(Vec<Spanned<PO>>),
    Invalid,
}

impl Display for BlankNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlankNode::Named(x) => write!(f, "_:{}", x),
            BlankNode::Unnamed(pos) => {
                if pos.len() == 0 {
                    write!(f, "[ ]")
                } else {
                    write!(f, "[ ")?;

                    for po in pos {
                        write!(f, "{} ;", po.value())?;
                    }

                    write!(f, " ]")
                }
            }
            BlankNode::Invalid => write!(f, "invalid"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Subject {
    BlankNode(BlankNode),
    NamedNode(NamedNode),
}

impl Display for Subject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Subject::BlankNode(b) => b.fmt(f),
            Subject::NamedNode(n) => n.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Literal(Literal),
    BlankNode(BlankNode),
    NamedNode(NamedNode),
    Collection(Vec<Spanned<Term>>),
    Invalid,
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Literal(l) => l.fmt(f),
            Term::BlankNode(b) => b.fmt(f),
            Term::NamedNode(n) => n.fmt(f),
            Term::Collection(n) => {
                write!(f, "( ")?;
                for l in n {
                    l.fmt(f)?;
                }
                write!(f, "  )")?;
                Ok(())
            }
            Term::Invalid => write!(f, "invalid"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Triple {
    pub subject: Spanned<Subject>,
    pub po: Vec<Spanned<PO>>,
}

impl Display for Triple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.subject.value(), self.po[0].value())?;

        for po in &self.po[1..] {
            write!(f, "; {}", po.value())?;
        }

        write!(f, ".")
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PO {
    pub predicate: Spanned<NamedNode>,
    pub object: Vec<Spanned<Term>>,
}

impl Display for PO {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.predicate.value(), self.object[0].value())?;

        for po in &self.object[1..] {
            write!(f, ", {}", po.value())?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Base(pub Range<usize>, pub Spanned<NamedNode>);
impl Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@base {} .", self.1.value())
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prefix {
    pub span: Range<usize>,
    pub prefix: Spanned<String>,
    pub value: Spanned<NamedNode>,
}
impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "@prefix {}: {} .",
            self.prefix.value(),
            self.value.value()
        )
    }
}

#[derive(Debug)]
pub enum TurtleSimpleError {
    Parse(IriParseError),
    UnexpectedBase(&'static str),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Turtle {
    pub base: Option<Spanned<Base>>,
    pub set_base: lsp_types::Url,
    pub prefixes: Vec<Spanned<Prefix>>,
    pub triples: Vec<Spanned<Triple>>,
}

impl Turtle {
    pub fn empty(location: &lsp_types::Url) -> Self {
        Self::new(None, Vec::new(), Vec::new(), location)
    }

    pub fn get_simple_triples<'a>(
        &'a self,
    ) -> Result<Vec<([MyTerm<'a>; 3], GraphName<MyTerm<'a>>)>, TurtleSimpleError> {
        let mut blank_nodes = 0;
        let mut out = Vec::new();

        let base = match &self.base {
            Some(Spanned(Base(_, Spanned(named_node, _)), _)) => {
                let nn = named_node.expand_step(self, HashSet::new()).ok_or(
                    TurtleSimpleError::UnexpectedBase("Expected valid named node base"),
                )?;
                BaseIri::new(nn).map_err(TurtleSimpleError::Parse)?
            }
            None => BaseIri::new(self.set_base.as_str().to_string())
                .map_err(TurtleSimpleError::Parse)?,
        };

        let mut todo = Vec::new();
        for Spanned(ref triple, _) in &self.triples {
            let sub = match triple.subject.value() {
                Subject::BlankNode(BlankNode::Named(vs)) => MyTerm::blank_node(vs),
                Subject::BlankNode(BlankNode::Unnamed(vs)) => {
                    blank_nodes += 1;
                    let out = MyTerm::blank_node(format!("internal_bnode_{}", blank_nodes));
                    todo.push((out.clone(), vs));
                    out
                }
                Subject::BlankNode(BlankNode::Invalid) => {
                    return Err(TurtleSimpleError::UnexpectedBase(
                        "Unexpected invalid blank node",
                    ))
                }
                Subject::NamedNode(nn) => MyTerm::named_node(
                    nn.expand_step(self, HashSet::new())
                        .ok_or(TurtleSimpleError::UnexpectedBase(
                            "Expected valid named node for object",
                        ))
                        .and_then(|n| {
                            base.resolve(n.as_str())
                                .map_err(|e| TurtleSimpleError::Parse(e))
                        })
                        .map(|x| x.unwrap())?,
                ),
            };
            todo.push((sub.clone(), &triple.po));
        }

        while let Some((sub, po)) = todo.pop() {
            for Spanned(PO { predicate, object }, _) in po {
                let predicate = MyTerm::named_node(
                    predicate
                        .value()
                        .expand_step(self, HashSet::new())
                        .ok_or(TurtleSimpleError::UnexpectedBase(
                            "Expected valid named node for object",
                        ))
                        .and_then(|n| {
                            base.resolve(n.as_str())
                                .map_err(|e| TurtleSimpleError::Parse(e))
                        })
                        .map(|x| x.unwrap())?,
                );

                for Spanned(term, _) in object {
                    let object = match term {
                        Term::NamedNode(nn) => MyTerm::named_node(
                            nn.expand_step(self, HashSet::new())
                                .ok_or(TurtleSimpleError::UnexpectedBase(
                                    "Expected valid named node for object",
                                ))
                                .and_then(|n| {
                                    base.resolve(n.as_str())
                                        .map_err(|e| TurtleSimpleError::Parse(e))
                                })
                                .map(|x| x.unwrap())?,
                        ),
                        Term::Literal(literal) => MyTerm::literal(literal.plain_string()),
                        Term::BlankNode(bn) => match bn {
                            BlankNode::Named(v) => MyTerm::blank_node(v),
                            BlankNode::Unnamed(v) => {
                                blank_nodes += 1;
                                let out =
                                    MyTerm::blank_node(format!("internal_bnode_{}", blank_nodes));
                                todo.push((out.clone(), v));
                                out
                            }
                            BlankNode::Invalid => {
                                return Err(TurtleSimpleError::UnexpectedBase(
                                    "Unexpected invalid blank for object",
                                ))
                            }
                        },
                        Term::Collection(_) => {
                            return Err(TurtleSimpleError::UnexpectedBase(
                                "Collection terms are not yet supported",
                            ))
                        }
                        Term::Invalid => {
                            return Err(TurtleSimpleError::UnexpectedBase(
                                "Unexpected invalid object",
                            ))
                        }
                    };
                    out.push(([sub.clone(), predicate.clone(), object], GraphName::None));
                }
            }
        }

        Ok(out)
    }
}

impl Turtle {
    pub fn new(
        base: Option<Spanned<Base>>,
        prefixes: Vec<Spanned<Prefix>>,
        triples: Vec<Spanned<Triple>>,
        location: &lsp_types::Url,
    ) -> Self {
        Self {
            base,
            prefixes,
            triples,
            set_base: location.clone(),
        }
    }

    pub fn get_base(&self) -> &lsp_types::Url {
        &self.set_base
    }
}

impl Display for Turtle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(b) = &self.base {
            writeln!(f, "{}", b.value())?;
        }

        self.prefixes
            .iter()
            .map(|x| x.value())
            .try_for_each(|x| writeln!(f, "{}", x))?;

        self.triples
            .iter()
            .map(|x| x.value())
            .try_for_each(|x| writeln!(f, "{}", x))?;

        Ok(())
    }
}

#[cfg(test)]
mod test {

    use std::str::FromStr;

    use chumsky::{Parser, Stream};

    use crate::{
        lang::turtle::{parser::turtle, tokenizer, Turtle},
        model::{spanned, Spanned},
    };

    #[derive(Debug)]
    pub enum Err {
        Tokenizing,
        Parsing,
    }

    fn parse_turtle(
        inp: &str,
        url: &lsp_types::Url,
    ) -> Result<(Turtle, Vec<Spanned<String>>), Err> {
        let tokens = tokenizer::parse_tokens().parse(inp).map_err(|err| {
            println!("Token error {:?}", err);
            Err::Tokenizing
        })?;
        let end = inp.len() - 1..inp.len() + 1;

        let mut comments: Vec<_> = tokens
            .iter()
            .filter(|x| x.0.is_comment())
            .cloned()
            .map(|x| spanned(x.0.to_comment(), x.1))
            .collect();
        comments.sort_by_key(|x| x.1.start);

        let stream = Stream::from_iter(end, tokens.into_iter().filter(|x| !x.0.is_comment()));

        turtle(&url)
            .parse(stream)
            .map_err(|err| {
                println!("Parse error {:?}", err);
                Err::Parsing
            })
            .map(|t| (t, comments))
    }

    #[test]
    fn easy_triples() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
# @base <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

[] a foaf:Name;
   foaf:knows <abc>;.
"#;

        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, _) = parse_turtle(txt, &url).expect("Simple");
        let triples = output.get_simple_triples().expect("Triples found");

        assert_eq!(triples.len(), 2);
        println!("{:?}", triples);
    }

    #[test]
    fn easy_triples_2() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
# @base <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

[ foaf:knows <abc>; ] 
    a foaf:Name;
    foaf:knows [
        a foaf:Name;
        foaf:knows [
            a foaf:Name; ] ].
"#;

        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, _) = parse_turtle(txt, &url).expect("Simple");
        let triples = output.get_simple_triples().expect("Triples found");

        assert_eq!(triples.len(), 6);
    }
}
