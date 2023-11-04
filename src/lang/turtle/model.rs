use hashbrown::HashSet;
use tracing::info;

use super::token::StringStyle;
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
    pub fn expand(&self, turtle: &Turtle, base: &str) -> Option<String> {
        let base = turtle.get_base(base);
        let out = self.expand_step(turtle, HashSet::new())?;

        let url = base.join(&out).ok()?;

        Some(url.to_string())
    }

    fn expand_step<'a>(&'a self, turtle: &Turtle, mut done: HashSet<&'a str>) -> Option<String> {
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

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Turtle {
    pub base: Option<Spanned<Base>>,
    set_base: Option<lsp_types::Url>,
    pub prefixes: Vec<Spanned<Prefix>>,
    pub triples: Vec<Spanned<Triple>>,
}

impl Turtle {
    pub fn new(
        base: Option<Spanned<Base>>,
        prefixes: Vec<Spanned<Prefix>>,
        triples: Vec<Spanned<Triple>>,
    ) -> Self {
        Self {
            base,
            prefixes,
            triples,
            set_base: None,
        }
    }
    pub fn set_base(&mut self, location: &str) {
        let location_url = lsp_types::Url::parse(location).unwrap();

        let base = self
            .base
            .as_ref()
            .and_then(|base| match base.0 .1.value() {
                NamedNode::Full(s) => Some(
                    location_url
                        .join(s.as_str())
                        .map(|x| x.to_string())
                        .unwrap_or(s.to_string()),
                ),
                _ => None,
            })
            .unwrap_or(location.to_string());

        self.set_base = lsp_types::Url::parse(&base).ok();
    }
    pub fn get_base(&self, location: &str) -> lsp_types::Url {
        self.set_base
            .clone()
            .unwrap_or_else(|| lsp_types::Url::parse(location).unwrap())
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
