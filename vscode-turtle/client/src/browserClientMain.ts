/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import { ExtensionContext, Uri, window, workspace } from "vscode";
import { LanguageClientOptions } from "vscode-languageclient";

import { LanguageClient } from "vscode-languageclient/browser";

let orange = window.createOutputChannel("Orange");
// this method is called when vs code is activated
export function activate(context: ExtensionContext) {
  console.log("lsp-web-extension-sample activated!");
  orange.appendLine("lsp-web-extension-sample activated!");

  /*
   * all except the code to create the language client in not browser specific
   * and could be shared with a regular (Node) extension
   */

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: "turtle" }],
    synchronize: {},
    initializationOptions: {},
  };

  const client = createWorkerLanguageClient(context, clientOptions);

  const disposable = client.start();
  context.subscriptions.push(disposable);

  client.onReady().then(() => {
    console.log("lsp-web-extension-sample server is ready");
    orange.appendLine("lsp-web-extension-sample is ready!");
  });
}

type CustomEvent = {
  ty: "readFile";
  path: string;
  id: string;
};
function createWorkerLanguageClient(
  context: ExtensionContext,
  clientOptions: LanguageClientOptions,
) {
  // Create a worker. The worker main file implements the language server.
  const serverMain = Uri.joinPath(
    context.extensionUri,
    "server/dist/browserServerMain.js",
  );
  const worker = new Worker(serverMain.toString(true));
  worker.addEventListener("message", async (event) => {
    try {
      const data: CustomEvent = JSON.parse(event.data);
      if (data.ty === "readFile") {
        orange.appendLine("Reading file " + data.path);
        const body = await new Promise<string>((res, rej) => {
          workspace.fs
            .readFile(Uri.file(data.path))
            .then((body) => res(new TextDecoder().decode(body)), rej);
        });
        orange.appendLine("Read file " + data.path);

        worker.postMessage({ ty: "readFile", body, id: data.id });
      }
    } catch (ex) {}
  });

  // create the language server client to communicate with the server running in the worker
  return new LanguageClient(
    "turtle language server",
    "turtle language server",
    clientOptions,
    worker,
  );
}
