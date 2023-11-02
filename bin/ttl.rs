use jsonld_language_server::backend::Backend;
use jsonld_language_server::lang::turtle::TurtleLang;
use jsonld_language_server::prefix::Prefixes;
use std::fs::File;
use std::io;
use tower_lsp::LspService;
use tower_lsp::Server;
use tracing::Level;
use tracing_subscriber::fmt;
use std::sync::Mutex;

#[tokio::main]
async fn main() {
    let target: Box<dyn io::Write + Send + Sync> = match File::create("/tmp/turtle-lsp.txt") {
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

    let prefix = Prefixes::new().await.expect("Initalize prefixes");

    let (service, socket) =
        LspService::build(|client| Backend::<_, TurtleLang>::new(client, prefix)).finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
