package chester.cli

import scala.language.experimental.genericNumberLiterals

object VersionInfo {
  private val fallback = "0.1.0-SNAPSHOT"

  val current: String =
    sys.props
      .get("chester.version")
      .orElse(sys.env.get("CHESTER_VERSION"))
      .getOrElse(fallback)
}
