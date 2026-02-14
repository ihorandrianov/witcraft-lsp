# WIT Language Support

Language support for WebAssembly Interface Types (WIT) powered by the witcraft-lsp server.

## Features
- Syntax highlighting for .wit files.
- Diagnostics, hover, go-to-definition, and find references.
- Rename and workspace symbols.
- Formatting, semantic tokens, inlay hints, and document symbols.

## Installation
Install from the VS Code Marketplace. The extension downloads and caches the witcraft-lsp server
binary on first use, or you can point it to a local build.

## Configuration
- `wit-lsp.server.path`: Path to the witcraft-lsp executable. If empty, the extension downloads and caches the server.
- `wit-lsp.server.releaseChannel`: Release channel for server downloads (default: `latest`).
- `wit-lsp.server.version`: Pin a specific server version (example: `v0.2.0`). Overrides the release channel.
- `wit-lsp.trace.server`: Trace LSP communication (`off`, `messages`, `verbose`).

## Troubleshooting
- If the server fails to download, verify GitHub access and the release asset naming scheme.
- Check VS Code output logs for "WIT Language Server".
- To enable verbose server logs, set `WITCRAFT_LSP_DEBUG=1` in the environment that launches VS Code.

## Release assets
The extension expects release assets named:
- `witcraft-lsp-${tag}-${platform}-${arch}.tar.gz` for macOS and Linux
- `witcraft-lsp-${tag}-${platform}-${arch}.zip` for Windows

Platforms are `macos`, `linux`, `windows` and architectures are `x64`, `arm64`.
