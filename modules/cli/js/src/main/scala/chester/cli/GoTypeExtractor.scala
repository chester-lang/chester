package chester.cli

import chester.tyck.JSImportSignature
import java.nio.file.Path
import scala.language.experimental.genericNumberLiterals

/** Go type extractor is not supported on Scala.js targets. */
object GoTypeExtractor {

  /** Return an empty map on Scala.js. */
  def extractPackages(packagePaths: Vector[String]): Map[String, JSImportSignature] =
    Map.empty

  /** No-op on Scala.js; writing files is not supported here. */
  def saveToJson(signatures: Map[String, JSImportSignature], outputPath: Path): Unit =
    throw new UnsupportedOperationException("Go type extraction is not supported on Scala.js targets")
}
