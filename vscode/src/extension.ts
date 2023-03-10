// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(_context: vscode.ExtensionContext) {
  const command: string | undefined = vscode.workspace.getConfiguration("jsonld-lsp").get("command");

  const serverOptions: ServerOptions = {
    command: command || 'jsonld-language-server', 
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      {
        language: 'jsonld',
      },
    ],
  };

  const client = new LanguageClient('jsonld language server', serverOptions, clientOptions);
  return client.start();
}

// This method is called when your extension is deactivated
export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
