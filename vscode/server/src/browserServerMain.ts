/*"---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import { createConnection, BrowserMessageReader, BrowserMessageWriter } from 'vscode-languageserver/browser';

import { InitializeParams, TextDocumentPositionParams, PrepareRenameParams, SemanticTokensParams, DidOpenTextDocumentParams, RenameParams } from 'vscode-languageserver';

import init, { WebClient, WebBackend, set_logger, set_diags} from 'jsonld-language-server';


let server: undefined | WebBackend;

/* browser specific setup code */

const messageReader = new BrowserMessageReader(self);
const messageWriter = new BrowserMessageWriter(self);

const connection = createConnection(messageReader, messageWriter);

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


	server = new WebBackend(new WebClient());

	const out = await server.initialize(params);

	if (out) {
		return out;
	}
});

connection.languages.semanticTokens.on(async (params: SemanticTokensParams) => {
	return await server?.semantic_tokens_full(params);
});

connection.onDidChangeTextDocument(async change => {
	await server?.did_change(change);
});

connection.onDidOpenTextDocument(async (x: DidOpenTextDocumentParams) => {
	return await server?.did_open(x);
});

// This handler provides the initial list of the completion items.
connection.onCompletion(async (position: TextDocumentPositionParams) => {
	return await server?.completion(position);
});

connection.onPrepareRename(async (x: PrepareRenameParams) => {
	return await server?.prepare_rename(x);
});

connection.onRenameRequest(async (x: RenameParams) => {
	return await server?.rename(x);
});

// Listen on the connection
connection.listen();
