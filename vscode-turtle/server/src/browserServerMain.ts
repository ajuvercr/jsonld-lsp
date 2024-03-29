/*"---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import {
  BrowserMessageReader,
  BrowserMessageWriter,
  createConnection,
} from "vscode-languageserver/browser";

import {
    CodeActionParams,
  DidOpenTextDocumentParams,
  InitializeParams,
  PrepareRenameParams,
  RenameParams,
  SemanticTokensParams,
  TextDocumentPositionParams,
} from "vscode-languageserver";

import init, {
  set_diags,
  set_logger,
  set_read_file,
  turtle_backend,
  TurtleWebBackend,
  WebClient,
} from "jsonld-language-server";
import { sign } from "crypto";
// import { readFile } from "fs/promises";

let server: undefined | TurtleWebBackend;

/* browser specific setup code */

const messageReader = new BrowserMessageReader(self);
const messageWriter = new BrowserMessageWriter(self);

const connection = createConnection(messageReader, messageWriter);

console.log = connection.console.log.bind(connection.console);
console.error = connection.console.error.bind(connection.console);

function log_message(msg: string) {
  connection.console.log(msg);
}

function publish_diagnostics(diags: any) {
  connection.console.log("Js: Publishing diagnostics " + JSON.stringify(diags));
  connection.sendDiagnostics(diags);
}

type CustomEvent = {
  ty: "readFile";
  path: string;
  body?: string;
  id: string;
};
let filesRead = 0;
connection.onInitialize(async (params: InitializeParams) => {
  console.log("init params " + JSON.stringify(params, undefined, 2));
  await init();

  set_logger(log_message);
  set_diags(publish_diagnostics);
  set_read_file((loc: string) => {
    return new Promise((res) => {
      console.log("Reading file!", loc);
      let id = filesRead + "";
      filesRead += 1;
      const data: CustomEvent = {
        id,
        path: loc,
        ty: "readFile",
      };
      // const body = await readFile(loc, { encoding: "utf8" });
      const controller = new AbortController();
      const signal = controller.signal;

      addEventListener(
        "message",
        (msg) => {
          console.log("Got message! " + JSON.stringify(msg.data));
          try {
            const data: CustomEvent = msg.data;
            if (data.ty == "readFile" && data.id == id) {
              console.log("Got file!");
              controller.abort();
              msg.stopPropagation();
              msg.stopImmediatePropagation();
              res(data.body!);
            }
          } catch (ex: any) {}
        },
        { signal },
      );

      postMessage(JSON.stringify(data));
    });
  });

  connection.console.log("Callbacks set!");

  server = await turtle_backend(new WebClient());

  const out = await server?.initialize(params);
  if (out) {
    connection.console.log("out! " + JSON.stringify(out, undefined, 2));
    return out;
  }
});

connection.languages.semanticTokens.on(async (params: SemanticTokensParams) => {
  connection.console.log("Semantic tokens!");
  return await server?.semantic_tokens_full(params);
});

connection.onDocumentFormatting(async (change) => {
  connection.console.log("Doc formatting");
  return await server?.formatting(change);
});

connection.onCodeAction(async (change: CodeActionParams) => {
  return await server?.code_action(change);
})

connection.onDidChangeTextDocument(async (change) => {
  connection.console.log("Doc change");
  await server?.did_change(change);
});

connection.onDidOpenTextDocument(async (x: DidOpenTextDocumentParams) => {
  connection.console.log("Doc open");
  return await server?.did_open(x);
});

// This handler provides the initial list of the completion items.
connection.onCompletion(async (position: TextDocumentPositionParams) => {
  connection.console.log("On completion!");
  const completionRes = await server?.completion(position);
  connection.console.log("Completion resp! " + JSON.stringify(completionRes));
  return completionRes;
});

connection.onPrepareRename(async (x: PrepareRenameParams) => {
  return await server?.prepare_rename(x);
});

connection.onRenameRequest(async (x: RenameParams) => {
  return await server?.rename(x);
});

// Listen on the connection
connection.listen();
