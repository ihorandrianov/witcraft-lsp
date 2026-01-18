use witcraft_lsp::WitLanguageServer;
use tower_lsp::{LspService, Server};
use tracing::info;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .init();

    info!("Starting WIT LSP server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(WitLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
