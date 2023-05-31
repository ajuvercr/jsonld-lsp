use std::collections::HashMap;

use crate::{
    lang::{Lang, Node, Token},
    lsp_types::{SemanticToken, SemanticTokenType},
    model::{spanned, Spanned},
    parent::ParentingSystem,
};
use ropey::Rope;

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
];

struct T {
    start: usize,
    length: usize,
    ty: usize,
}

pub fn semantic_tokens<L: Lang>(
    lang: &L,
    system: &ParentingSystem<Spanned<L::Node>>,
    rope: &Rope,
) -> Vec<SemanticToken> {
    let mut tokens: HashMap<_, _> = system
        .iter()
        .flat_map(|(_, x)| x.leaf().map(|t| spanned(t, x.span().clone())))
        .flat_map(|s| s.value().token().map(|t| (s.span().clone(), t)))
        .collect();

    lang.semantic_tokens(system, |token, ty| {
        tokens.insert(token, ty);
    });

    let tokens: Vec<_> = tokens
        .into_iter()
        .map(|(k, v)| T {
            start: k.start,
            length: k.end - k.start,
            ty: LEGEND_TYPE.iter().position(|x| x == &v).unwrap_or(0),
        })
        .collect();

    tokens.sort_by(|a, b| a.start.cmp(&b.start));

    let mut pre_line = 0;
    let mut pre_start = 0;

    tokens
        .into_iter()
        .flat_map(|token| {
            let line = rope.try_byte_to_line(token.start as usize).ok()? as u32;
            let first = rope.try_line_to_char(line as usize).ok()? as u32;
            let start = rope.try_byte_to_char(token.start as usize).ok()? as u32 - first;
            let delta_line = line - pre_line;
            let delta_start = if delta_line == 0 {
                start - pre_start
            } else {
                start
            };
            let ret = Some(SemanticToken {
                delta_line,
                delta_start,
                length: token.length as u32,
                token_type: token.ty as u32,
                token_modifiers_bitset: 0,
            });
            pre_line = line;
            pre_start = start;
            ret
        })
        .collect()
}
