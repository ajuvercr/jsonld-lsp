use jsonld_language_server::backend::Backend;
use jsonld_language_server::lang::jsonld::JsonLd;
use std::fs::File;
use std::io;
use std::sync::Mutex;
use tower_lsp::LspService;
use tower_lsp::Server;
use tracing::Level;
use tracing_subscriber::fmt;

#[tokio::main]
async fn main() {
    let target: Box<dyn io::Write + Send + Sync> = match File::create("/tmp/json_ld-lsp.txt") {
        Ok(x) => Box::new(x),
        Err(_) => Box::new(std::io::stdout()),
    };

    fmt()
        .with_file(true)
        .with_line_number(true)
        .with_max_level(Level::DEBUG)
        .with_writer(Mutex::new(target))
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) =
        LspService::build(|client| Backend::<_, JsonLd>::new(client, Default::default())).finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
