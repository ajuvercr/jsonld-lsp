use std::ops::Range;

use common::Token;
use enum_methods::{EnumIntoGetters, EnumIsA, EnumToGetters};
use lsp_types::SemanticTokenType;

use super::{Base, BlankNode, Literal, NamedNode, Prefix, Term, Triple, Turtle};
use crate::{
    lang::common,
    model::{spanned, Spanned},
    parent::ParentingSystem,
};

#[derive(Clone, Debug, PartialEq, EnumIntoGetters, EnumIsA, EnumToGetters)]
pub enum Leaf {
    Literal(Literal),
    NamedNode(NamedNode),
    BlankNode(String),
    Invalid,
}

impl Token for Leaf {
    fn token(&self) -> Option<lsp_types::SemanticTokenType> {
        let t = match self {
            Leaf::Literal(lit) => match lit {
                Literal::RDF(_) => SemanticTokenType::STRING,
                Literal::Boolean(_) => SemanticTokenType::ENUM_MEMBER,
                Literal::Numeric(_) => SemanticTokenType::NUMBER,
            },
            Leaf::NamedNode(_) => SemanticTokenType::FUNCTION,
            Leaf::BlankNode(_) => SemanticTokenType::STRING,
            Leaf::Invalid => return None,
        };
        Some(t)
    }
}

impl Into<Leaf> for Literal {
    fn into(self) -> Leaf {
        Leaf::Literal(self)
    }
}

impl Into<Leaf> for NamedNode {
    fn into(self) -> Leaf {
        Leaf::NamedNode(self)
    }
}

impl Into<Leaf> for String {
    fn into(self) -> Leaf {
        Leaf::BlankNode(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PO {
    pub predicate: usize,
    pub objects: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq, EnumIntoGetters, EnumIsA, EnumToGetters)]
pub enum Node {
    Leaf(Leaf),
    Triple { subject: usize, po: Vec<PO> },
    BlankNode { po: Vec<PO> },
    Base(Range<usize>, usize),
    Prefix(Range<usize>, Spanned<String>, usize),
    Root(Vec<usize>),
    Collection(Vec<usize>),
    Invalid,
}

impl Node {
    pub fn ty_str(&self) -> &'static str {
        match self {
            Node::Leaf(Leaf::Invalid) => "leaf(invalid)",
            Node::Leaf(Leaf::Literal(_)) => "leaf(literal)",
            Node::Leaf(Leaf::NamedNode(_)) => "leaf(namednode)",
            Node::Leaf(Leaf::BlankNode(_)) => "leaf(blanknod)",
            Node::Triple { .. } => "triple",
            Node::BlankNode { .. } => "blanknode",
            Node::Base(_, _) => "base",
            Node::Prefix(_, _, _) => "prefix",
            Node::Root(_) => "root",
            Node::Collection(_) => "collection",
            Node::Invalid => "invalid",
        }
    }
}

impl common::Node<Leaf> for Node {
    fn leaf<'a>(&'a self) -> Option<&'a Leaf> {
        match self {
            Node::Leaf(leaf) => Some(leaf),
            _ => None,
        }
    }
}

impl ParentingSystem<Spanned<Node>> {
    fn increment(&mut self, parent: usize) -> usize {
        self.add(spanned(Node::Invalid, 0..0), parent)
    }

    fn add_leaf<N: Into<Leaf>>(&mut self, Spanned(nn, span): Spanned<N>, parent: usize) -> usize {
        let element = self.increment(parent);
        self.objects[element] = spanned(Node::Leaf(nn.into()), span);
        element
    }

    fn add_base(
        &mut self,
        Spanned(Base(span, nn), total_span): Spanned<Base>,
        parent: usize,
    ) -> usize {
        let element = self.increment(parent);
        let child = self.add_leaf(nn, element);
        self.objects[element] = spanned(Node::Base(span, child), total_span);
        element
    }

    fn add_prefix(
        &mut self,
        Spanned(
            Prefix {
                span,
                prefix,
                value,
            },
            total_span,
        ): Spanned<Prefix>,
        parent: usize,
    ) -> usize {
        let element = self.increment(parent);
        let child = self.add_leaf(value, element);
        self.objects[element] = spanned(Node::Prefix(span, prefix, child), total_span);
        element
    }

    fn add_term(&mut self, Spanned(term, span): Spanned<Term>, parent: usize) -> usize {
        match term {
            Term::Literal(x) => self.add_leaf(Spanned(x, span), parent),
            Term::NamedNode(x) => self.add_leaf(Spanned(x, span), parent),
            Term::BlankNode(x) => self.add_bnode(Spanned(x, span), parent),
            Term::Collection(x) => self.add_collection(Spanned(x, span), parent),
            Term::Invalid => todo!(),
        }
    }

    fn add_collection(
        &mut self,
        Spanned(terms, span): Spanned<Vec<Spanned<Term>>>,
        parent: usize,
    ) -> usize {
        let element = self.increment(parent);

        let nodes: Vec<_> = terms
            .into_iter()
            .map(|t| self.add_term(t, element))
            .collect();

        self.objects[element] = spanned(Node::Collection(nodes), span);
        element
    }

    fn add_bnode(&mut self, Spanned(bnode, span): Spanned<BlankNode>, parent: usize) -> usize {
        let element = self.increment(parent);

        let po = match bnode {
            BlankNode::Named(b) => return self.add_leaf(Spanned(b, span), element),
            BlankNode::Unnamed(po) => po,
            BlankNode::Invalid => todo!(),
        };

        let children = self.extract_po(po, element);
        self.objects[element] = spanned(Node::BlankNode { po: children }, span);
        element
    }

    fn add_triple(
        &mut self,
        Spanned(Triple { subject, po }, total_span): Spanned<Triple>,
        parent: usize,
    ) -> usize {
        let element = self.increment(parent);
        let subject = match subject.0 {
            Term::BlankNode(b) => self.add_bnode(Spanned(b, subject.1), element),
            Term::NamedNode(n) => self.add_leaf(Spanned(n, subject.1), element),
            _ => self.add_leaf(Spanned(NamedNode::Invalid, subject.1), element),
        };

        let po = self.extract_po(po, element);
        self.objects[element] = spanned(Node::Triple { subject, po }, total_span);

        element
    }

    fn extract_po(&mut self, po: Vec<Spanned<super::model::PO>>, parent: usize) -> Vec<PO> {
        let mut children = Vec::new();
        for Spanned(p, __) in po {
            let nn = self.add_leaf(
                p.predicate
                    .map(|x| x.named_node().unwrap_or(&NamedNode::Invalid).clone()),
                parent,
            );
            let objects = p
                .object
                .into_iter()
                .map(|x| self.add_term(x, parent))
                .collect();

            children.push(PO {
                predicate: nn,
                objects,
            });
        }
        children
    }

    pub fn new_turtle(Spanned(turtle, span): Spanned<Turtle>) -> Self {
        let mut this = Self::new();
        let root_id = this.increment(0);
        let mut root = Vec::new();

        if let Some(b) = turtle.base {
            root.push(this.add_base(b, root_id));
        }

        for prefix in turtle.prefixes {
            root.push(this.add_prefix(prefix, root_id));
        }

        for triple in turtle.triples {
            root.push(this.add_triple(triple, root_id));
        }

        this.objects[root_id] = spanned(Node::Root(root), span.clone());
        this
    }
}
