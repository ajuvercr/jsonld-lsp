use lsp_types::Range;

use crate::lang::SimpleCompletion;

use super::Turtle;

#[async_trait::async_trait]
pub trait CompletionProvider<Ctx> {
    type State: Clone;

    fn new(state: &Self::State) -> Self;
    async fn update(
        &self,
        turtle: &Turtle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>>;

    async fn find_completions(&self, ctx: &Ctx, range: Range) -> Vec<SimpleCompletion>;
}

mod namespace;
pub use namespace::*;

mod shape;
pub use shape::*;
