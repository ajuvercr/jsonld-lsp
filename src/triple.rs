use crate::lang::turtle::{self, BlankNode, Turtle};

pub struct Triple {
    pub subject: String,
    pub predicate: String,
    pub object: String,
}

pub fn extract_triples<S>(turtle: &Turtle) -> Vec<Triple> {
    let mut triples: Vec<Triple> = Vec::new();

    for triple in &turtle.triples {
        let subject = match &triple.subject.0 {
            turtle::Term::BlankNode(BlankNode::Named(x)) => x.clone(),
            turtle::Term::NamedNode(n) => {
                if let Some(x) = n.expand(&turtle) {
                    // info!("subject {:?} -> {}", n, x);
                    x
                } else {
                    continue;
                }
            }
            _ => continue,
        };

        for po in &triple.po {
            if let Some(pred) = po.predicate.expand(&turtle) {
                for ob in &po.object {
                    let object = match &ob.0 {
                        turtle::Term::Literal(s) => s.plain_string(),
                        turtle::Term::BlankNode(BlankNode::Named(x)) => x.clone(),
                        turtle::Term::NamedNode(n) => {
                            if let Some(o) = n.expand(&turtle) {
                                o
                            } else {
                                continue;
                            }
                        }
                        _ => continue,
                    };
                    triples.push(Triple {
                        subject: subject.clone(),
                        predicate: pred.clone(),
                        object,
                    });
                }
            } else {
                continue;
            }
        }
    }

    triples
}
