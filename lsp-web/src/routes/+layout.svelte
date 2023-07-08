<script lang="ts">
  import "@picocss/pico";
  import Highlight from "svelte-highlight";
  import lua from "svelte-highlight/languages/lua";
  import "svelte-highlight/styles/github.css";

  const code = `#  Add a config to lspconfig.configs
local configs = require("lspconfig.configs")

configs.jsonld = {
  default_config = {
    cmd = { 'jsonld-language-server' },
    filetypes = { 'jsonld' },
    root_dir = require("lspconfig.util")
      .find_git_ancestor,
    single_file_support = true,
    init_options = {},
  }
}

# Start the LSP
local lspconfig = require("lspconfig")

lspconfig.jsonld.setup {
  on_attach = M.on_attach,
  capabilities = M.capabilities,
}`;
</script>

<br />
<header class="container">
  <hgroup>
    <h1>JSON-LD Language Server Demo</h1>
    <h2>
      Simple demo application that shows the power and usefulness of adding LSP
      support to JSON-LD.
    </h2>
  </hgroup>
</header>

<main class="container">
  <section id="editor">
    <h2>Editor</h2>
    <slot />
    <br />
    <p>
      The editor will provide autocompletion on properties defined in the linked
      person.jsonld context. This will be done very greedily but still a
      productivity boost is present.
    </p>
    <p>
      Autocompletion on defined identifiers can be issued by typing <strong
        >@</strong
      >.
    </p>
    <p>Syntax errors are also gathered by the language server.</p>
    <p>
      This editor is powered by the json-ld. The editor itself uses the
      codemirror components. A thin wrapper links the codemirror editor with the
      LSP. The LSP itself is compiled to WebAssembly and loaded on the page.
    </p>
    <p>
      Note that codemirror does not natively support LSPs, the LSP features you
      see here are coded with a small wrapper layer, but features like semantic
      highlighting are not supported for this reason.
    </p>
  </section>

  <section id="screencast">
    <h2>Screencast</h2>
    Because of the double blind requirements I'm not able to let you integrate the
    LSP with your local editors, this however does give the best experience. To help
    you better understand what it looks like in a real editor, I created a screencast
    of the LSP in VSCode.

    <img
      src="./screen.gif"
      alt="Screencast"
    />
  </section>
  <section id="install">
    <h2>Editor compatibility</h2>
    The main benifit of a LSP is that it can be installed on many platforms. NeoVim
    only requires some configuration because the Language Server protocol is native
    to the editor. Other editors like VSCode require a little bit more work to, to
    create an extension for each supported Language Server, this is considered to
    be a thin wrapper.

    <div class="flow">
      <article class="article">
        <img
          class="logo"
          src="https://www.shanebart.com/wp-content/uploads/2019/05/5k4h36j3h4j.png"
          alt="Visual Studio Code Logo"
        />
        <h2>VS Code</h2>
        <p>
          Installing the language server for VS Code is as simple as installing
          the correct extensions. This extension is called <em>redacted</em> lsp
          and can be found on <em>redacted</em>.
        </p>
        <p>
          Creating the extension is done with special care to make it a, so
          called, web extension. This makes it also possible to use this
          extension on online VS code instances like <a
            target="_blank"
            href="https://vscode.dev">vscode.dev</a
          >.
        </p>
      </article>
      <article class="article">
        <img
          class="logo"
          src="https://upload.wikimedia.org/wikipedia/commons/thumb/4/4f/Neovim-logo.svg/2560px-Neovim-logo.svg.png"
          alt="Neovim Logo"
        />
        <h2>NeoVim</h2>
        <p>
          NeoVim has native support for Language Servers. NeoVim starts up a
          binary and communicates with it via stdin and stdout. To enable the
          JSON-LD LSP, please install it with cargo. The NeoVim lsp integration
          has a dependency on the LSP binary.
        </p>
        <details>
          <summary>Lua configuration</summary>
          <Highlight language={lua} {code} />
        </details>
      </article>
    </div>
  </section>
</main>

<style>
  .logo {
    padding: 1em;
    margin-bottom: 1em;
  }
  .flow {
    display: flex;
    flex-wrap: wrap;
    justify-content: space-evenly;
    gap: 2em;
  }

  .flow * {
    flex: 520px;
  }
</style>
