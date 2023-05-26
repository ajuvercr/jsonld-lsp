# JSON-LD Language Server and language definition

Pretty basic JSON-LD language server. 

This language server enables autocompletion on defined properties from the current JSON-LD context.
It also enables autocompletion on defined ids in the current file and allows renaming these ids.

## Structure

The main language server is written in Rust and can be found at the root of this repository.
To build the VSCode extension the server needs to be compiled to WASM with the following command `wasm-pack build -t web --no-default-features`, this creates the pkg directory.

Next, the extension can be built and debugged.
- Run `npm install` in this folder. This installs all necessary npm modules in both the client and server folder
- Open VS Code on this folder.
- Press Ctrl+Shift+B to compile the client and server.
- Switch to the Debug viewlet.
- Select `Launch Client` from the drop down.
- Run the launch config.


### Code structure 

- __./client/src/browserClientMain.ts__ communicates with the current VSCode instance, when the extension is activated (__onLanguage:jsonld__) it starts the server code with a WebWorker and starts a Language Server Client that communicates with the WebWorker.
- __./server/src/browserServerMain.ts__ is a thin wrapper around the WASM code but is in itself the actual Language Server. When the server gets the initialize command it dynamically loads the WASM and starts the WebBackend and forwards the request. After that, all request the server gets are passed along to the WASM instance.

Note: Webpack inlines the WASM binary so that it bundles more easily.

```
.
├── client // Language Client
│   ├── src
│   │   └── browserClientMain.ts // Language Client entry point
├── package.json // The extension manifest.
└── server // Language Server
    └── src
        └── browserServerMain.ts // Language Server entry point
```

