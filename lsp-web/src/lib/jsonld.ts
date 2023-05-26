
import type { CompletionContext, CompletionResult } from "@codemirror/autocomplete";
import {
  LRLanguage, LanguageSupport,
  indentNodeProp,
  foldNodeProp, foldInside,
  continuedIndent
} from "@codemirror/language";

import { parser } from "@lezer/json"

/// A language provider that provides JSON parsing.
export const jsonLanguage = LRLanguage.define({
  name: "json",
  parser: parser.configure({
    props: [
      indentNodeProp.add({
        Object: continuedIndent({ except: /^\s*\}/ }),
        Array: continuedIndent({ except: /^\s*\]/ })
      }),
      foldNodeProp.add({
        "Object Array": foldInside
      })
    ]
  }),
  languageData: {
    closeBrackets: { brackets: ["[", "{", '"'] },
    indentOnInput: /^\s*[\}\]]$/
  }
});

function localCompletionSource(context: CompletionContext): CompletionResult | null {
  console.log("local completion!");
  let word = context.matchBefore(/\w*/)!;
  if (word.from == word.to && !context.explicit)
    return null
  return {
    from: word.from,
    options: [
      { label: "match", type: "keyword" },
      { label: "hello", type: "variable", info: "(World)" },
      { label: "magic", type: "text", apply: "⠁⭒*.✩.*⭒⠁", detail: "macro" }
    ]
  }
}


export function jsonld(): LanguageSupport {
  return new LanguageSupport(
    jsonLanguage,
    [
      jsonLanguage.data.of({
        autocomplete: localCompletionSource
      })
    ]
  );
}


