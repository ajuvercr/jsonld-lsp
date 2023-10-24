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
  turtle_backend,
  TurtleWebBackend,
  WebClient,
} from "jsonld-language-server";

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


connection.onInitialize(async (params: InitializeParams) => {
  console.log("init params " + JSON.stringify(params, undefined, 2));
  await init();

  set_logger(log_message);
  set_diags(publish_diagnostics);

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
  connection.console.log("Completion resp! " +  JSON.stringify(completionRes));
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
