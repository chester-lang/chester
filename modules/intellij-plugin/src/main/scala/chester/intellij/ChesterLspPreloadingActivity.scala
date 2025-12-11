package chester.intellij

import com.intellij.openapi.application.PreloadingActivity
import com.intellij.openapi.progress.ProgressIndicator
import org.wso2.lsp4intellij.IntellijLanguageClient
import org.wso2.lsp4intellij.client.languageserver.serverdefinition.RawCommandServerDefinition

/** Registers Chester LSP for .chester files on IDE startup. */
class ChesterLspPreloadingActivity extends PreloadingActivity:
  override def preload(indicator: ProgressIndicator): Unit =
    register()

  override def preload(): Unit =
    register()

  private def register(): Unit =
    val command = Array("chester-lsp")
    val definition = new RawCommandServerDefinition("chester", command)
    IntellijLanguageClient.addServerDefinition(definition)
