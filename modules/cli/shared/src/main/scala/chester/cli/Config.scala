package chester.cli

import scala.language.experimental.genericNumberLiterals

enum Config:
  case Run(file: Option[String])
  case Compile(input: String, output: Option[String])
  case Version
  case Help
