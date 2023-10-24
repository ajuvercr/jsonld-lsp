/*"---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import { createConnection, BrowserMessageReader, BrowserMessageWriter } from 'vscode-languageserver/browser';

import { InitializeParams, TextDocumentPositionParams, PrepareRenameParams, SemanticTokensParams, DidOpenTextDocumentParams, RenameParams } from 'vscode-languageserver';

import init, { WebClient, JsonLDWebBackend, set_logger, set_diags} from 'jsonld-language-server';


let server: undefined | JsonLDWebBackend;

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
	await init();

	set_logger(log_message);
	set_diags(publish_diagnostics);

	connection.console.log("Callbacks set!");

  server = new JsonLDWebBackend(new WebClient());

	const out = await server.initialize(params);

	if (out) {
		return out;
	}
});

connection.languages.semanticTokens.on(async (params: SemanticTokensParams) => {
	return await server?.semantic_tokens_full(params);
});

connection.onDocumentFormatting(async change => {
	return await server?.formatting(change);
});

connection.onDidChangeTextDocument(async change => {
	await server?.did_change(change);
});

connection.onDidOpenTextDocument(async (x: DidOpenTextDocumentParams) => {
	return await server?.did_open(x);
});

// This handler provides the initial list of the completion items.
connection.onCompletion(async (position: TextDocumentPositionParams) => {
	const out = await server?.completion(position);
  return out;
});

connection.onPrepareRename(async (x: PrepareRenameParams) => {
	return await server?.prepare_rename(x);
});

connection.onRenameRequest(async (x: RenameParams) => {
	return await server?.rename(x);
});

// Listen on the connection
connection.listen();
