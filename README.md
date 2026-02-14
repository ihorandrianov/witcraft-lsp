# wit-lsp

Language Server Protocol implementation for WIT (WebAssembly Interface Types), plus editor integration assets.

## Repository layout

- `crates/witcraft-syntax`: lexer, parser, formatter, symbol index, and syntax tests.
- `crates/witcraft-lsp`: LSP server implementation (`witcraft-lsp` binary).
- `editors/vscode`: VS Code extension that starts and configures the server.
- `examples`: sample `.wit` files used for development and tests.

## Features

- Parsing and diagnostics for WIT files.
- Navigation: definition, references, hover, document/workspace symbols.
- Editing support: rename, formatting, semantic tokens, code actions, inlay hints.
- Cross-file and workspace-level indexing for package-aware resolution.

## Build and test

From repository root:

```bash
cargo test
cargo build --release -p witcraft-lsp
```

VS Code extension checks:

```bash
npm --prefix editors/vscode ci
npm --prefix editors/vscode run typecheck
npm --prefix editors/vscode run package
```

## Release assets

Release workflow publishes platform-specific `witcraft-lsp` binaries and checksums from Git tags (`v*`), and also packages a VSIX extension artifact.

See `RELEASE_CHECKLIST.md` for the expected release process.

## License

This project is dual-licensed under MIT and Apache-2.0 where applicable. See crate metadata and `editors/vscode/LICENSE`.
