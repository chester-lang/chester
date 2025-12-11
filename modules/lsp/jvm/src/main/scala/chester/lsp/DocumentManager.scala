package chester.lsp

import java.net.URI
import java.nio.file.{Files, Paths}

import scala.collection.mutable
import scala.language.experimental.genericNumberLiterals
import scala.util.Try

class DocumentManager:
  private val openDocs = mutable.Map.empty[String, String]

  def open(uri: String, text: String): Unit =
    openDocs.update(uri, text)

  def update(uri: String, text: String): Unit =
    openDocs.update(uri, text)

  def close(uri: String): Unit =
    openDocs.remove(uri)

  def analyze(uri: String): Either[Seq[String], DocumentAnalysis] =
    textFor(uri).toRight(Seq(s"Document '$uri' is not available")).flatMap { text =>
      DocumentAnalyzer.analyze(uri, text)
    }

  private def textFor(uri: String): Option[String] =
    openDocs.get(uri).orElse(readFromDisk(uri))

  private def readFromDisk(uri: String): Option[String] =
    val maybePath = Try(Paths.get(new URI(uri))).orElse(Try(Paths.get(uri)))
    maybePath.toOption.flatMap(path => Try(new String(Files.readAllBytes(path))).toOption)
