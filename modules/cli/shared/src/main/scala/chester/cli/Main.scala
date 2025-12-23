package chester.cli

import scala.language.experimental.genericNumberLiterals

import chester.utils.io.{DefaultTerminal, given}
import chester.utils.io.impl.given

object Main {
  val progName: String = "chester"

  val usage: String = {
    s"""$progName - Chester CLI
       |
       |Usage:
       |  $progName [run] [file]                 Start the REPL or type-check a file
       |  $progName compile <file> [--output <path>]  Type-check and emit the elaborated AST
       |  $progName ts <file|dir> [--output <path>]   Type-check and emit TypeScript for a file or directory
       |  $progName go <file|dir> [--output <path>] [--go-sigs <file>]  Type-check and emit Go for a file or directory
       |  $progName go-extract <packages...> [--output <file>]  Extract Go package type signatures
       |  $progName format <file>                Format a Chester source file in-place
       |  $progName version                      Show version information
       |  $progName help                         Show this help message
       |""".stripMargin
  }

  def main(args: Array[String]): Unit = {
    parse(args.toList) match {
      case Right(config) =>
        CLI.run(config)
      case Left(message) =>
        System.err.println(message)
        System.err.println()
        System.err.println(usage)
        sys.exit(1)
    }
  }

  private def parse(args: List[String]): Either[String, Config] = args match {
    case Nil => Right(Config.Run(None))
    case ("-h" | "--help" | "help") :: _ =>
      Right(Config.Help)
    case "version" :: _ =>
      Right(Config.Version)
    case "run" :: rest =>
      parseRun(rest)
    case "compile" :: rest =>
      parseCompile(rest)
    case "ts" :: rest =>
      parseCompileTS(rest)
    case "go" :: rest =>
      parseCompileGo(rest)
    case "go-extract" :: rest =>
      parseExtractGoTypes(rest)
    case "format" :: rest =>
      parseFormat(rest)
    case head :: Nil if !head.startsWith("-") =>
      Right(Config.Run(Some(head)))
    case other =>
      val bad = other.headOption.getOrElse("")
      Left(s"Unknown command or option: '$bad'")
  }

  private def parseRun(args: List[String]): Either[String, Config] = args match {
    case Nil =>
      Right(Config.Run(None))
    case file :: Nil =>
      Right(Config.Run(Some(file)))
    case _ =>
      Left("run accepts at most one file path")
  }

  private def parseCompile(args: List[String]): Either[String, Config] = {
    def loop(rest: List[String], output: Option[String], input: Option[String]): Either[String, Config] = {
      rest match {
        case Nil =>
          input match {
            case Some(in) => Right(Config.Compile(in, output))
            case None     => Left("compile requires an input file")
          }
        case ("--output" | "-o") :: value :: tail =>
          loop(tail, Some(value), input)
        case ("--output" | "-o") :: Nil =>
          Left("compile option --output requires a value")
        case opt :: _ if opt.startsWith("-") =>
          Left(s"Unknown compile option: $opt")
        case value :: tail =>
          input match {
            case None    => loop(tail, output, Some(value))
            case Some(_) => Left("compile accepts only one input file")
          }
      }
    }

    loop(args, output = None, input = None)
  }

  private def parseCompileTS(args: List[String]): Either[String, Config] = {
    def loop(rest: List[String], output: Option[String], input: Option[String]): Either[String, Config] = rest match
      case Nil =>
        input match
          case Some(in) => Right(Config.CompileTS(in, output))
          case None     => Left("ts requires an input file or directory")
      case ("--output" | "-o") :: value :: tail =>
        loop(tail, Some(value), input)
      case ("--output" | "-o") :: Nil =>
        Left("ts option --output requires a value")
      case opt :: _ if opt.startsWith("-") =>
        Left(s"Unknown ts option: $opt")
      case value :: tail =>
        input match
          case None    => loop(tail, output, Some(value))
          case Some(_) => Left("ts accepts only one input path")

    loop(args, output = None, input = None)
  }

  private def parseCompileGo(args: List[String]): Either[String, Config] = {
    def loop(rest: List[String], output: Option[String], input: Option[String], goSigs: Option[String]): Either[String, Config] = rest match
      case Nil =>
        input match
          case Some(in) => Right(Config.CompileGo(in, output, goSigs))
          case None     => Left("go requires an input file or directory")
      case ("--output" | "-o") :: value :: tail =>
        loop(tail, Some(value), input, goSigs)
      case ("--output" | "-o") :: Nil =>
        Left("go option --output requires a value")
      case "--go-sigs" :: value :: tail =>
        loop(tail, output, input, Some(value))
      case "--go-sigs" :: Nil =>
        Left("go option --go-sigs requires a value")
      case opt :: _ if opt.startsWith("-") =>
        Left(s"Unknown go option: $opt")
      case value :: tail =>
        input match
          case None    => loop(tail, output, Some(value), goSigs)
          case Some(_) => Left("go accepts only one input path")

    loop(args, output = None, input = None, goSigs = None)
  }

  private def parseExtractGoTypes(args: List[String]): Either[String, Config] = {
    def loop(rest: List[String], output: Option[String], packages: Vector[String]): Either[String, Config] = rest match
      case Nil =>
        if packages.isEmpty then Left("go-extract requires at least one package name")
        else Right(Config.ExtractGoTypes(packages, output))
      case ("--output" | "-o") :: value :: tail =>
        loop(tail, Some(value), packages)
      case ("--output" | "-o") :: Nil =>
        Left("go-extract option --output requires a value")
      case opt :: _ if opt.startsWith("-") =>
        Left(s"Unknown go-extract option: $opt")
      case value :: tail =>
        loop(tail, output, packages :+ value)

    loop(args, output = None, packages = Vector.empty)
  }

  private def parseFormat(args: List[String]): Either[String, Config] = args match {
    case file :: Nil => Right(Config.Format(file))
    case Nil         => Left("format requires an input file")
    case _           => Left("format accepts only one input file")
  }
}
