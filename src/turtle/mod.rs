mod parser;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Literal {
    value: String,
    lang: Option<String>,
    ty: Option<NamedNode>,
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (&self.lang, &self.ty) {
            (None, None) => write!(f, "\"{}\"", self.value),
            (None, Some(t)) => write!(f, "\"{}\"^^{}", self.value, t),
            (Some(l), None) => write!(f, "\"{}\"@{}", self.value, l),
            (Some(l), Some(t)) => write!(f, "\"{}\"@{}^^{}", self.value, l, t),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NamedNode {
    Full(String),
    Prefixed { prefix: String, value: String },
    A,
}
impl Display for NamedNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamedNode::Full(x) => write!(f, "<{}>", x),
            NamedNode::Prefixed { prefix, value } => write!(f, "{}:{}", prefix, value),
            NamedNode::A => write!(f, "a"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlankNode(pub String);
impl Display for BlankNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_:{}", self.0)
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
}
impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Literal(l) => l.fmt(f),
            Term::BlankNode(b) => b.fmt(f),
            Term::NamedNode(n) => n.fmt(f),
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
    pub object: Vec<Spanned<O>>,
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
pub enum O {
    Term(Spanned<Term>),
    Object(Vec<Spanned<PO>>),
}

impl O {
    pub fn is_term(&self) -> bool {
        match self {
            O::Term(_) => true,
            O::Object(_) => false,
        }
    }

    pub fn is_bn(&self) -> bool {
        match self {
            O::Term(_) => false,
            O::Object(_) => true,
        }
    }
}
impl Display for O {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            O::Term(t) => t.fmt(f),
            O::Object(o) => {
                write!(f, "[ {}", o[0].value())?;

                for p in &o[1..] {
                    write!(f, "; {}", p.value())?;
                }

                write!(f, " ]")
            }
        }
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Turtle {
    base: Option<Spanned<Base>>,
    prefixes: Vec<Spanned<Prefix>>,
    triples: Vec<Spanned<Triple>>,
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

use std::fmt::Display;

pub use parser::*;

use crate::model::Spanned;
