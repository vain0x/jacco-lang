import { ExtensionContext, workspace, commands } from "vscode"
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient"
// import { homedir } from "os"
// import * as path from "path"

let client: LanguageClient | undefined

/** LSP サーバーの実行ファイルの絶対パスを取得する。 */
const getLspBin = (context: ExtensionContext): string | null => {
  const config = workspace.getConfiguration("jacco-lang")

  const useLsp = config.get<boolean>("use-lsp", true)
  if (useLsp === false) {
    return null
  }

  if (process.env.JACCO_LSP_BIN) {
    return process.env.JACCO_LSP_BIN
  }

  const relativePath =
    config.get("lsp-bin") as string | undefined
    ?? "./out/jacco-lsp"
  return context.asAbsolutePath(relativePath)
}

// const getJaccoHome = () => {
//   const config = workspace.getConfiguration("jacco-lang")
//   return config.get("jacco-home") as string | undefined
//     || process.env.JACCO_HOME
//     || path.join(homedir(), "bin/jacco")
// }

const startLspClient = (context: ExtensionContext) => {
  const lspBin = getLspBin(context)
  if (lspBin == null) {
    return
  }

  // const workspaceUris = (workspace.workspaceFolders ?? [])
  //   .map(workspaceFolder => workspaceFolder.uri.toString())

  let serverOptions: ServerOptions = {
    command: lspBin,
    args: [
      // "--workspace-count",
      // workspaceUris.length.toString(),
      // ...workspaceUris,
      // "--log-path",
      // context.logPath,
      "start",
    ],
  }

  let clientOptions: LanguageClientOptions = {
    documentSelector: [
      {
        language: "jacco",
      },
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
  }

  // LSP サーバーとクライアントを起動する。
  client = new LanguageClient(
    "jacco",
    "Jacco Language",
    serverOptions,
    clientOptions,
  )
  client.start()
}

/** 拡張機能の起動時に呼ばれる。 */
export const activate = (context: ExtensionContext): void => {
  // commands.registerCommand("getLspBin", () => getLspBin(context))

  startLspClient(context)
}

/** 拡張機能の終了時に呼ばれる。 */
export const deactivate = (): Thenable<void> | undefined => {
  if (client) {
    return client.stop()
  }
}
