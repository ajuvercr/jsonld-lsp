use jsonld_language_server::backend::Backend;
use jsonld_language_server::lang::jsonld::JsonLd;
use log::LevelFilter;
use std::fs::File;
use tower_lsp::LspService;
use tower_lsp::Server;

#[tokio::main]
async fn main() {
    let target = match File::create("/tmp/json_ld-lsp.txt") {
        Ok(x) => env_logger::Target::Pipe(Box::new(x)),
        Err(_) => env_logger::Target::Stderr,
    };

    env_logger::Builder::from_default_env()
        .target(target)
        .init();

    log::set_max_level(LevelFilter::Trace);

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) =
        LspService::build(|client| Backend::<_, JsonLd>::new(client, Default::default())).finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
