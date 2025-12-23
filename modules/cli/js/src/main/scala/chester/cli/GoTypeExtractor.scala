package chester.cli

import chester.tyck.{JSImportSignature}

/** Go type extractor is not supported on Scala.js targets. */
object GoTypeExtractor {

  /** Return an empty map on Scala.js. */
  def extractPackages(packagePaths: Vector[String]): Map[String, JSImportSignature] = {
    if (packagePaths.nonEmpty) {
      System.err.println("Go type extraction is not supported on Scala.js targets; skipping extraction.")
    }
    Map.empty
  }

  /** No-op on Scala.js; writing files is not supported here. */
  def saveToJson(signatures: Map[String, JSImportSignature], outputPath: String): Unit =
    System.err.println("Go type extraction is not supported on Scala.js targets; skipping write.")
}
