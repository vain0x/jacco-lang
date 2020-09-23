import {
  CancellationToken,
  DocumentRangeSemanticTokensProvider,
  DocumentSemanticTokensProvider,
  Event,
  ExtensionContext,
  languages,
  ProviderResult,
  Range,
  SemanticTokens,
  SemanticTokensBuilder,
  SemanticTokensLegend,
  TextDocument,
} from "vscode"

const legend: SemanticTokensLegend = {
  tokenTypes: ["bad", "variable", "function", "type"],
  tokenModifiers: [],
}

const tokenize = (text: string): SemanticTokens => {
  const builder = new SemanticTokensBuilder(legend)

  for (const { row, line } of text.split(/\r?\n/).map((line, row) => ({ line, row }))) {
    let i = 0
    for (const word of line.split(/\\b/)) {
      if (/^[0-9A-Za-z_]+$/.test(word)) {
        const tokenType = word.includes("Type")
          ? 3 : (
            word.includes("_fn") ? 2 : 1
          )

        console.log("token", { row, column: i, length: word.length, word, tokenType })
        builder.push(row, i, word.length, tokenType)
      } else {
        console.log("not a word", word)
      }

      i += word.length
    }
  }

  return builder.build()
}

class MySemanticTokensProvider implements DocumentSemanticTokensProvider, DocumentRangeSemanticTokensProvider {
  provideDocumentRangeSemanticTokens(document: TextDocument, range: Range, token: CancellationToken): ProviderResult<SemanticTokens> {
    console.log("provideDocumentRangeSemanticTokens", range)
    return tokenize(document.getText(range))
  }

  // onDidChangeSemanticTokens?: Event<void> | undefined

  provideDocumentSemanticTokens(document: TextDocument, token: CancellationToken): ProviderResult<SemanticTokens> {
    console.log("provideDocumentSemanticTokens")
    return tokenize(document.getText())
  }
}

export const registerSemanticTokensProvider = (context: ExtensionContext) => {
  context.subscriptions.push(
    languages.registerDocumentSemanticTokensProvider(
      {
        language: "jacco",
      },
      new MySemanticTokensProvider(),
      legend,
    ))
}
