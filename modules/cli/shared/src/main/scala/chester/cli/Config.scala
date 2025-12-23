package chester.cli

import scala.language.experimental.genericNumberLiterals

enum Config:
  case Run(file: Option[String])
  case Compile(input: String, output: Option[String])
  case CompileTS(input: String, output: Option[String])
  case CompileGo(input: String, output: Option[String], goSigs: Option[String])
  case ExtractGoTypes(packages: Vector[String], output: Option[String])
  case Format(input: String)
  case Version
  case Help
