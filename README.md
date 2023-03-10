# JSON-LD LSP 
 
## Introduction

Welcome.

This repo includes the source code for this JSON-LD Language Server.


## Use the LSP

To use the LSP you will always have to install the binary.
So do that first:

```
cargo install --path . --bin jsonld-language-server
```

**NVIM**

To use the lsp on NVIM, use the lspconfig.

```lua
#  Add a config to lspconfig.configs
local configs = require("lspconfig.configs")

configs.jsonld = {
  default_config = {
    cmd = { 'jsonld-language-server' },
    filetypes = { 'jsonld' },
    root_dir = require("lspconfig.util").find_git_ancestor,
    single_file_support = true,
    init_options = {},
  }
}

# Start the LSP
local lspconfig = require("lspconfig")

lspconfig.jsonld.setup {
  on_attach = M.on_attach,
  capabilities = M.capabilities,
}
```


**VS Code**

Install the vscode extension 'jsonld lsp'.

