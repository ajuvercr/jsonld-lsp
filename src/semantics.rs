use crate::lsp_types::{SemanticToken, SemanticTokenType};
use ropey::Rope;

use crate::model::{JsonToken, ParentingSystem};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::PROPERTY,
];

struct Token {
    ty: usize,
    start: usize,
    length: usize,
}

pub fn semantic_tokens(system: &ParentingSystem, rope: &Rope) -> Vec<SemanticToken> {
    let mut tokens: Vec<_> = system
        .iter()
        .flat_map(|(_, x)| {
            let mut span = x.span();
            let ty = match x.value() {
                JsonToken::KV(key, _) => {
                    span = key.span();
                    match key.as_str() {
                        "@id" | "@type" | "@context" => SemanticTokenType::KEYWORD,
                        _ => SemanticTokenType::PROPERTY,
                    }
                }
                JsonToken::Str(_) => SemanticTokenType::STRING,
                JsonToken::Bool(_) => SemanticTokenType::NUMBER,
                JsonToken::Num(_) => SemanticTokenType::NUMBER,
                _ => return None,
            };
            Some((span, ty))
        })
        .map(|(span, ty)| Token {
            ty: LEGEND_TYPE.iter().position(|x| x == &ty).unwrap(),
            start: span.start,
            length: span.len(),
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
