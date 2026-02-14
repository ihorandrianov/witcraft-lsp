# Witcraft LSP

Witcraft LSP provides Language Server Protocol support for WIT (WebAssembly Interface Types) files.

## Install

### VS Code

Install the extension from the VS Code Marketplace or Open VSX.

The extension downloads the correct `witcraft-lsp` binary on first launch and caches it in the extension storage. You can override the server path in settings.

Settings:

- `wit-lsp.server.path`: Use a local `witcraft-lsp` binary instead of downloading.
- `wit-lsp.server.releaseChannel`: Release channel to download (default: `latest`).
- `wit-lsp.server.version`: Pin a specific version (example: `v0.2.0`).

### Neovim

1) Download the archive for your platform from GitHub Releases.
2) Extract the `witcraft-lsp` binary into a directory on your PATH.
3) Ensure it is executable (`chmod +x` on macOS/Linux).

Minimal `nvim-lspconfig` setup:

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.witcraft_lsp then
  configs.witcraft_lsp = {
    default_config = {
      cmd = { 'witcraft-lsp' },
      filetypes = { 'wit' },
      root_dir = lspconfig.util.root_pattern('.git'),
    },
  }
end

lspconfig.witcraft_lsp.setup({})
```

## Manual Install

Release assets are published per platform:

- `witcraft-lsp-vX.Y.Z-macos-x64.tar.gz`
- `witcraft-lsp-vX.Y.Z-macos-arm64.tar.gz`
- `witcraft-lsp-vX.Y.Z-linux-x64.tar.gz`
- `witcraft-lsp-vX.Y.Z-linux-arm64.tar.gz`
- `witcraft-lsp-vX.Y.Z-windows-x64.zip`

Checksum verification:

```bash
shasum -a 256 -c SHA256SUMS
```

## Troubleshooting

- If the extension cannot download the server, set `wit-lsp.server.path` to a local binary.
- If you use a proxy or restricted network, download the release asset manually and configure `wit-lsp.server.path`.
