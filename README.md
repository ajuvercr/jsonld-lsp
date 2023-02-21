# boilerplate for a  rust language server powered by `tower-lsp` 
## Introduction
This repo is a template for `tower-lsp`, a useful github project template which makes writing new language servers easier.
## Development
1. `pnpm i`
2. `cargo build`
3. press <kbd>F5</kbd> or change to the Debug panel and click <kbd>Launch Client</kbd>
## A valid program in nano rust 
```rust
fn factorial(x) {
    // Conditionals are supported!
    if x == 0 {
        1
    } else {
        x * factorial(x - 1)
    }
}

// The main function
fn main() {
    let three = 3;
    let meaning_of_life = three * 14 + 1;

    print("Hello, world!");
    print("The meaning of life is...");

    if meaning_of_life == 42 {
        print(meaning_of_life);
    } else {
        print("...something we cannot know");

        print("However, I can tell you that the factorial of 10 is...");
        // Function calling
        print(factorial(10));
    }
}
```
## Features
This repo use a language `nano rust` which first introduced by [ chumsky ](https://github.com/zesterer/chumsky/blob/master/examples/nano_rust.rs). Most common language feature has been implemented, you could preview via the video below.

- [x] InlayHint for LiteralType
![inlay hint](https://user-images.githubusercontent.com/17974631/156926412-c3823dac-664e-430e-96c1-c003a86eabb2.gif)

- [x] semantic token   
make sure your semantic token is enabled, you could enable your `semantic token` by
adding this line  to your `settings.json`
```json
{
 "editor.semanticHighlighting.enabled": true,
}
```
- [x] syntactic error diagnostic

https://user-images.githubusercontent.com/17974631/156926382-a1c4c911-7ea1-4d3a-8e08-3cf7271da170.mp4

- [x] code completion  

https://user-images.githubusercontent.com/17974631/156926355-010ef2cd-1d04-435b-bd1e-8b0dab9f44f1.mp4

- [x] go to definition  

https://user-images.githubusercontent.com/17974631/156926103-94d90bd3-f31c-44e7-a2ce-4ddfde89bc33.mp4

- [x] find reference

https://user-images.githubusercontent.com/17974631/157367235-7091a36c-631a-4347-9c1e-a3b78db81714.mp4

- [x] rename

https://user-images.githubusercontent.com/17974631/157367229-99903896-5583-4f67-a6da-1ae1cf206876.mp4


## Add to nvim

```lua
local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")
configs.jsonld = {
  default_config = {
    cmd = { 'jsonld-language-server' },
    filetypes = { 'jsonld' },
    root_dir = require("lspconfig.util").find_git_ancestor,
    single_file_support = true,
    init_options = {},
    autostart = true,
  },
  autostart = true,
}
lspconfig.jsonld.setup {
  on_attach = astronvim.lsp.on_attach,
  autostart = true,
}
```





