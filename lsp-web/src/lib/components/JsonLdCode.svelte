<script lang="ts">
  import type { Extension } from "@codemirror/state";
  import { syntaxTree } from "@codemirror/language";
  import { linter } from "@codemirror/lint";
  import type { Diagnostic as D } from "@codemirror/lint";
  import type { EditorView } from "@codemirror/view";

  import CodeMirror from "svelte-codemirror-editor";
  import { onMount } from "svelte";
  import init, {
    WebBackend,
    WebClient,
    set_diags,
    set_logger,
    init_panic_hook,
  } from "jsonld-language-server";
  import type { Diagnostic } from "vscode-languageserver";
  import { jsonLanguage, jsonld } from "$lib/jsonld";
  import type {
    CompletionResult,
    CompletionContext,
    Completion,
  } from "@codemirror/autocomplete";

  export let value: string = "";

  const documentUri = "file://document.jsonld";

  let server: WebBackend | undefined;

  let version = 0;

  type Diagnostics = { diagnostics: Diagnostic[]; uri: string };
  function update_value(value: string) {
    if (server) {
      version += 1;
      server.did_change({
        textDocument: { uri: documentUri, version },
        contentChanges: [{ text: value }],
      });
    }
  }

  let diagnosticsBuffer: Diagnostic[] = [];
  function log_msg(msg: string) {
    console.log("MSG: ", msg);
  }

  function do_set_diags(msg: Diagnostics) {
    diagnosticsBuffer = msg.diagnostics;
  }

  async function localCompletionSource(
    context: CompletionContext
  ): Promise<CompletionResult | null> {
    let nodeBefore = syntaxTree(context.state).resolveInner(context.pos, -1);
    let textBefore = context.state.sliceDoc(nodeBefore.from, context.pos);
    let tagBefore = /[^\s"@]*$/.exec(textBefore);
    const json = context.state.doc.toJSON();

    const lenghts = json.map((x) => x.length);
    const numToPos = (pos: number) => {
      let line = 0;
      while (pos > lenghts[line]) {
        pos -= lenghts[line] + 1;
        line += 1;
      }
      return {
        character: pos,
        line,
      };
    };

    const positionToNum = (p: { line: number; character: number }) => {
      let out = p.character;
      lenghts.slice(0, p.line).forEach((x) => {
        out += x + 1;
      });
      return out;
    };

    const position = numToPos(context.pos);
    const triggerCharacter = json[position.line].charAt(position.character - 1);

    const res = await server?.completion({
      context: {
        triggerKind:  2,//context.explicit ? 1 : 2,
        triggerCharacter,
      },
      textDocument: { uri: documentUri },
      position,
    });

    if (!res) return null;

    const options: Completion[] = [];
    for (let c of res) {
      options.push({
        label: c.filterText || c.textEdit.newText,
        type: c.kind === 6 ? "variable" : "property", 
        apply: c.textEdit?.newText,
        detail: c.documentation
      });
    }


    const out = {
      from: tagBefore ? nodeBefore.from + tagBefore.index - 1 : context.pos - 1,
      options,
      // options: tagOptions,
      validFor: /^([^\s"@]*)?$/,
    };
    return out;
  }

  const linterExtension = linter((view) => {
    let diagnostics: D[] = [];
    const local = diagnosticsBuffer.slice();
    diagnosticsBuffer = [];

    const lenghts = view.state.doc.toJSON().map((x) => x.length);
    const positionToNum = (p: { line: number; character: number }) => {
      let out = p.character;
      lenghts.slice(0, p.line).forEach((x) => {
        out += x + 1;
      });
      return out;
    };

    for (let d of local) {
      const to = positionToNum(d.range.end);
      const from = positionToNum(d.range.start);
      diagnostics.push({
        message: d.message,
        to,
        from,
        severity: "error",
      });
    }

    return diagnostics;
  });

  const extensions: Extension[] = [
    linterExtension,
    jsonLanguage.data.of({
      autocomplete: localCompletionSource,
    }),
  ];

  onMount(async () => {
    await init();
    init_panic_hook();
    set_logger(log_msg);
    set_diags(do_set_diags);

    const client = new WebClient();
    server = new WebBackend(client);

    server.did_open({
      textDocument: {
        uri: documentUri,
        version,
        languageId: "jsonld",
        text: value,
      },
    });
  });

  $: update_value(value);
</script>

<CodeMirror bind:value lang={jsonld()} {extensions} />
