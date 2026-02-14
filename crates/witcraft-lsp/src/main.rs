use tower_lsp::{LspService, Server};
use tracing::info;
use witcraft_lsp::WitLanguageServer;

const DEBUG_ENV: &str = "WITCRAFT_LSP_DEBUG";

#[tokio::main]
async fn main() {
    let debug_enabled = std::env::var(DEBUG_ENV)
        .map(|value| value == "1" || value.eq_ignore_ascii_case("true"))
        .unwrap_or(false);
    let builder = tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .with_max_level(if debug_enabled {
            tracing::Level::DEBUG
        } else {
            tracing::Level::INFO
        });
    builder.init();

    info!("Starting WIT LSP server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(WitLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
