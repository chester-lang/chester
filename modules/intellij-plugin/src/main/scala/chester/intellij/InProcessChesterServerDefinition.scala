package chester.intellij

import chester.lsp.ChesterLanguageServer
import org.eclipse.lsp4j.launch.LSPLauncher
import org.wso2.lsp4intellij.client.connection.StreamConnectionProvider
import org.wso2.lsp4intellij.client.languageserver.serverdefinition.LanguageServerDefinition

import java.io.{InputStream, OutputStream, PipedInputStream, PipedOutputStream}
import java.util.concurrent.Executors
import scala.language.experimental.genericNumberLiterals

/** Stream provider that runs the Chester LSP in-process and bridges via piped streams. */
private class InProcessChesterConnectionProvider extends StreamConnectionProvider:
  private val clientInput = PipedInputStream()
  private val clientOutput = PipedOutputStream()
  private val serverInput = PipedInputStream(clientOutput)
  private val serverOutput = PipedOutputStream(clientInput)

  private val executor = Executors.newSingleThreadExecutor { r =>
    val t = Thread(r, "chester-lsp-in-process")
    t.setDaemon(true)
    t
  }

  override def start(): Unit = {
    executor.submit(new Runnable {
      override def run(): Unit = {
        val server = ChesterLanguageServer()
        val launcher = LSPLauncher.createServerLauncher(server, serverInput, serverOutput)
        server.connect(launcher.getRemoteProxy)
        launcher.startListening().get()
      }
    })
  }

  override def getInputStream: InputStream = clientInput

  override def getOutputStream: OutputStream = clientOutput

  override def stop(): Unit = {
    try serverInput.close()
    catch case _: Throwable => ()
    try serverOutput.close()
    catch case _: Throwable => ()
    executor.shutdownNow()
  }

/** Language server definition that reuses Chester LSP directly from the plugin classpath. */
case class InProcessChesterServerDefinition(language: String) extends LanguageServerDefinition:
  ext = language
  override def createConnectionProvider(ext: String): StreamConnectionProvider =
    InProcessChesterConnectionProvider()
