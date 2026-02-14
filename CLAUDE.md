# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Witcraft LSP is a Language Server Protocol implementation for WIT (WebAssembly Interface Types) files. WIT is the interface definition language for the WebAssembly Component Model.

## Build Commands

```bash
# Build all crates
cargo build

# Build release
cargo build --release

# Run tests
cargo test --lib

# Run specific test module
cargo test index::tests

# Run parser benchmark
cargo run --release -p witcraft-syntax --example parse

# Run the LSP server
cargo run --release -p witcraft-lsp
```

## Architecture

```
crates/
├── witcraft-syntax/  # Parser, lexer, AST, symbol indexing
└── witcraft-lsp/     # LSP server using tower-lsp

editors/
└── vscode/           # VS Code extension (launches witcraft-lsp binary via stdio)
```

### witcraft-syntax crate

Core parsing library with these key modules:

- `lexer.rs` - `logos`-based tokenizer, never fails
- `parser.rs` - Recursive descent parser with error recovery
- `ast.rs` - AST types: `SourceFile`, `InterfaceDecl`, `WorldDecl`, type definitions
- `index.rs` - Symbol indexing for definitions and references
- `lookup.rs` - Find AST node at byte offset
- `text.rs` - `LineIndex` for offset ↔ line/column conversion

**Design principles:**
- Lossless syntax tree preserving whitespace and comments
- Error recovery: continues parsing after errors using synchronization points (`}`, `;`, item keywords)
- Byte offsets throughout, converted to LSP positions via `LineIndex`

### witcraft-lsp crate

- `server.rs` - `LanguageServer` trait implementation with all LSP handlers
- `document/store.rs` - Concurrent document store using `DashMap`

**Implemented LSP features:** diagnostics, semantic tokens, hover, go-to-definition, find references, completions, document symbols, formatting

## Testing

Test files live alongside source in `src/`. Run with `cargo test --lib`.

Integration tests use real WASI WIT files in `examples/wasi/`.

## VS Code Extension

In `editors/vscode/`. Registers `.wit` files and spawns the `witcraft-lsp` binary.

## Key Limitations

- Full document sync only (no incremental updates)
