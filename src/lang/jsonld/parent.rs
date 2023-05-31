use enum_methods::{EnumIntoGetters, EnumIsA, EnumToGetters};

use crate::{model::Spanned, parent::ParentingSystem};

use super::{parser::Json, tokenizer::JsonToken};

#[derive(Clone, Debug, PartialEq, EnumIntoGetters, EnumIsA, EnumToGetters)]
pub enum JsonNode {
    Leaf(JsonToken),
    Array(Vec<usize>),
    Object(Vec<usize>),
    Kv(Spanned<String>, usize),
}

pub fn system(element: Spanned<Json>) -> ParentingSystem<Spanned<JsonNode>> {
    let mut system = ParentingSystem::new();
    add(element, 0, &mut system);
    system
}

fn add(
    Spanned(el, span): Spanned<Json>,
    parent: usize,
    system: &mut ParentingSystem<Spanned<JsonNode>>,
) -> Option<usize> {
    match el {
        Json::Invalid => return None,
        Json::Array(jsons) => {
            let children: Vec<_> = jsons
                .into_iter()
                .map(|x| add(x, 0, system))
                .flatten()
                .collect();
            let this = system.add(Spanned(JsonNode::Array(children.clone()), span), parent);
            children
                .into_iter()
                .for_each(|i| system.set_parent(i, this));
            return Some(this);
        }
        Json::Object(ks) => {
            let mut children = Vec::new();
            for Spanned((k, v), kv_span) in ks {
                if let Some(child) = add(v, 0, system) {
                    let kv = JsonNode::Kv(k, child);
                    let kv_child = system.add(Spanned(kv, kv_span), 0);
                    system.set_parent(child, kv_child);
                    children.push(kv_child);
                }
            }

            let this = system.add(Spanned(JsonNode::Object(children.clone()), span), parent);
            children
                .into_iter()
                .for_each(|i| system.set_parent(i, this));
            return Some(this);
        }
        Json::Token(i) => {
            let this = system.add(Spanned(JsonNode::Leaf(i), span), parent);
            return Some(this);
        }
    }
}
