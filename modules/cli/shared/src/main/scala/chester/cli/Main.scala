package chester.cli

import scala.language.experimental.genericNumberLiterals

import chester.utils.io.{DefaultTerminal, given}
import chester.utils.io.impl.given

object Main {
  val progName: String = "chester"

  val usage: String =
    s"""$progName - Chester CLI
       |
       |Usage:
       |  $progName [run] [file]                 Start the REPL or type-check a file
       |  $progName compile <file> [--output <path>]  Type-check and emit the elaborated AST
       |  $progName version                      Show version information
       |  $progName help                         Show this help message
       |""".stripMargin

  def main(args: Array[String]): Unit =
    parse(args.toList) match {
      case Right(config) =>
        CLI.run(config)
      case Left(message) =>
        System.err.println(message)
        System.err.println()
        System.err.println(usage)
        sys.exit(1)
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
    def loop(rest: List[String], output: Option[String], input: Option[String]): Either[String, Config] =
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

    loop(args, output = None, input = None)
  }
}
