use crate::model::Spanned;
use std::fmt::Display;

pub mod tokenizer;
// pub mod parser;
mod parser;
mod token;

pub use parser::*;

use self::token::StringStyle;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    RDF(RDFLiteral),
    Boolean(bool),
    Numeric(String),
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
    value: String,
    quote_style: StringStyle,
    lang: Option<String>,
    ty: Option<NamedNode>,
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
    Invalid,
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Literal(l) => l.fmt(f),
            Term::BlankNode(b) => b.fmt(f),
            Term::NamedNode(n) => n.fmt(f),
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
pub struct Base(pub Spanned<NamedNode>);
impl Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@base {} .", self.0.value())
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prefix {
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
    pub prefixes: Vec<Spanned<Prefix>>,
    pub triples: Vec<Spanned<Triple>>,
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
