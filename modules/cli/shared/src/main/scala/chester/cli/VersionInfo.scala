package chester.cli

import scala.language.experimental.genericNumberLiterals

object VersionInfo {
  private val fallback = "0.1.0-SNAPSHOT"

  val current: String =
    Option(getClass.getPackage.getImplementationVersion).getOrElse(fallback)
}
