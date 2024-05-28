use jsonld_language_server::backend::Backend;
use jsonld_language_server::lang::turtle::TurtleLang;
use jsonld_language_server::prefix::Prefixes;
use std::fs::File;
use std::io;
use std::sync::Mutex;
use tower_lsp::LspService;
use tower_lsp::Server;
use tracing::info;
use tracing::Level;
use tracing_subscriber::fmt;

#[tokio::main(flavor = "multi_thread", worker_threads = 10)]
async fn main() {
    let target: Box<dyn io::Write + Send + Sync> = match File::create("/tmp/turtle-lsp.txt") {
        Ok(x) => Box::new(x),
        Err(_) => Box::new(std::io::stdout()),
    };

    std::panic::set_hook(Box::new(|_panic_info| {
        let backtrace = std::backtrace::Backtrace::capture();
        info!("My backtrace: {:#?}", backtrace);
    }));
    fmt()
        .with_file(true)
        .with_line_number(true)
        .with_max_level(Level::INFO)
        .with_writer(Mutex::new(target))
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    info!("Creating prefixes");
    let prefix = Prefixes::new().await.expect("Initalize prefixes");
    info!("Created prefixes");

    let (service, socket) = LspService::build(|client| {
        Backend::<_, TurtleLang>::new(client, (prefix, Default::default(), Default::default()))
    })
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
