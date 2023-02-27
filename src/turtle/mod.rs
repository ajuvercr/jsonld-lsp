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
pub struct NamedNode(pub String);
impl Display for NamedNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", self.0)
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
            },
        }
    }
}

use std::fmt::Display;

pub use parser::*;

use crate::model::Spanned;
