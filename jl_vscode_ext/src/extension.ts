import { homedir } from "os"
import * as path from "path"
import { ExtensionContext, workspace } from "vscode"
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient"
import { registerSemanticTokensProvider } from "./semantic_token_provider"

let client: LanguageClient | undefined

type Logger = {
  info: (...args: unknown[]) => void
}

/**
 * ジャッコ言語のインストールディレクトリを見つける。
 */
const getJaccoHome = (logger: Logger): string | null => {
  const config = workspace.getConfiguration("jacco-lang")

  const home = config.get<string>("home")
  if (home) {
    return home
  }
  logger.info("設定 'jacco-lang.home' は空または未設定です。")

  if (process.env.JACCO_HOME) {
    return process.env.JACCO_HOME
  }
  logger.info("環境変数 'JACCO_HOME' は空または未設定です。")

  return path.join(homedir(), ".jacco")
}

/**
 * LSP サーバーの実行ファイルを見つける。
 */
const getLspBin = (jaccoHome: string, logger: Logger): string | null => {
  const config = workspace.getConfiguration("jacco-lang")

  const useLsp = config.get<boolean>("use-lsp", true)
  if (!useLsp) {
    logger.info("設定 'jacco-lang.use-lsp' が false なので LSP サーバーを起動しません。")
    return null
  }

  const lspBin = config.get("lsp-bin") as string | undefined
  if (lspBin) {
    return lspBin
  }
  logger.info("設定 'jacco-lang.lsp-bin' は空または未設定です。")

  if (process.env.JACCO_LSP_BIN) {
    return process.env.JACCO_LSP_BIN
  }
  logger.info("環境変数 'JACCO_LSP_BIN' は空または未設定です。")

  return path.join(jaccoHome, "bin/jacco_lsp")
}

const startLspClient = (_context: ExtensionContext, logger: Logger) => {
  const jaccoHome = getJaccoHome(logger)
  if (jaccoHome == null) {
    return
  }
  logger.info("jaccoHome =", jaccoHome)

  const lspBin = getLspBin(jaccoHome, logger)
  if (lspBin == null) {
    return
  }
  logger.info("lspBin =", lspBin)

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

/**
 * 拡張機能の起動時に呼ばれる。
 */
export const activate = (context: ExtensionContext): void => {
  // commands.registerCommand("getLspBin", () => getLspBin(context))

  const logger: Logger =  {
    info: (...args) => console.info("[jacco-lsp]", ...args),
  }

  registerSemanticTokensProvider(context)
  startLspClient(context, logger)
}

/**
 * 拡張機能の終了時に呼ばれる。
 */
export const deactivate = (): Thenable<void> | undefined => {
  if (client) {
    return client.stop()
  }
}
