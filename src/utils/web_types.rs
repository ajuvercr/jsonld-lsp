use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen(typescript_custom_section)]
const ITEXT_STYLE: &'static str = r#"

import { DidChangeTextDocumentParams, DidSaveTextDocumentParams, DidOpenTextDocumentParams, TextDocumentPositionParams, InitializeParams, PrepareRenameParams, RenameParams, SemanticTokensParams, Diagnostic, CompletionParams, CodeActionParams, DocumentFormattingParams } from "vscode-languageserver";


type Diagnostics = { diagnostics: Diagnostic[]; uri: string };
type SetDiagnosticsFn = (diagnostics: Diagnostics) => void;
type SetLoggerFn = (msg: string) => void;
type SetReadFileFn = (url: string) => Promise<string>;
"#;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "CodeActionParams")]
    pub type CodeActionParams;

    #[wasm_bindgen(typescript_type = "InitializeParams")]
    pub type InitializeParams;

    #[wasm_bindgen(typescript_type = "CompletionParams")]
    pub type CompletionParams;

    #[wasm_bindgen(typescript_type = "RenameParams")]
    pub type RenameParams;

    #[wasm_bindgen(typescript_type = "PrepareRenameParams")]
    pub type PrepareRenameParams;

    #[wasm_bindgen(typescript_type = "SemanticTokensParams")]
    pub type SemanticTokensParams;

    #[wasm_bindgen(typescript_type = "DidChangeTextDocumentParams")]
    pub type DidChangeTextDocumentParams;

    #[wasm_bindgen(typescript_type = "CompletionItem")]
    pub type CompletionItem;

    #[wasm_bindgen(typescript_type = "DidOpenTextDocumentParams")]
    pub type DidOpenTextDocumentParams;

    #[wasm_bindgen(typescript_type = "TextDocumentPositionParams")]
    pub type TextDocumentPositionParams;

    #[wasm_bindgen(typescript_type = "DidSaveTextDocumentParams")]
    pub type DidSaveTextDocumentParams;

    #[wasm_bindgen(typescript_type = "SetDiagnosticsFn")]
    pub type SetDiagnosticsFn;

    #[wasm_bindgen(typescript_type = "SetLoggerFn")]
    pub type SetLoggerFn;

    #[wasm_bindgen(typescript_type = "SetReadFileFn")]
    pub type SetReadFileFn;


    #[wasm_bindgen(typescript_type = "DocumentFormattingParams")]
    pub type DocumentFormattingParams;
}
