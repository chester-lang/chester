package chester.intellij

import com.intellij.openapi.project.Project
import com.intellij.openapi.startup.StartupActivity
import org.wso2.lsp4intellij.IntellijLanguageClient
import scala.language.experimental.genericNumberLiterals

/** Registers Chester LSP for .chester files on IDE startup. */
class ChesterLspStartupActivity extends StartupActivity.DumbAware:
  override def runActivity(project: Project): Unit =
    IntellijLanguageClient.addServerDefinition(InProcessChesterServerDefinition("chester"))
