package chester.cli

import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.error.*
import chester.reader.{CharReader, FileNameAndContent, ParseError, Parser, Source, Tokenizer}
import chester.tyck.{ElabConstraint, ElabContext, ElabHandlerConf, ElabProblem, substituteSolutions}
import chester.utils.doc.DocConf
import chester.utils.elab.ProceduralSolverModule
import chester.utils.io.*
import chester.utils.term.*
import chester.utils.term.InputStatus.Complete
import chester.utils.term.{EndOfFile, LineRead, ReadLineResult, StatusError, UserInterrupted}

class CLI[F[_]](using runner: Runner[F], terminal: Terminal[F], io: IO[F]) {
  private given DocConf = DocConf.Default

  private object ReplInfo extends TerminalInfo {
    override def checkInputStatus(input: String): InputStatus = Complete
    override val defaultPrompt: fansi.Str = fansi.Str("chester> ")
    override val continuationPrompt: fansi.Str = fansi.Str("... ")
  }

  private def renderProblems(problems: Seq[Problem], source: Source): Seq[String] = {
    val reader = source.readContent.toOption
      .map(content => SourceReader.fromFileContent(FileContent(content, source.offset)))
      .getOrElse(SourceReader.empty)
    problems.map(p => p.renderDoc(using summon[DocConf], reader).toString)
  }

  private def analyze(source: Source): Either[Seq[String], (AST, Option[AST])] = {
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val parsed = for {
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    } yield Parser.parseFile(tokens)

    val parseProblems = parseReporter.getReports
    if (parseProblems.nonEmpty) {
      Left(renderProblems(parseProblems, source))
    } else {
      parsed match {
        case Left(err) =>
          Left(renderProblems(Seq(err), source))
        case Right(cst) =>
          val module = ProceduralSolverModule
          val solver = module.makeSolver[ElabConstraint](new ElabHandlerConf(module))
          val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
          val tyCell = module.newOnceCell[ElabConstraint, AST](solver)
          val ctx = ElabContext(bindings = Map.empty, types = Map.empty, reporter = elabReporter)

          module.addConstraint(solver, ElabConstraint.InferTopLevel(cst, resultCell, tyCell, ctx))
          module.run(solver)

          val elabProblems = elabReporter.getReports
          if (elabProblems.nonEmpty) {
            Left(renderProblems(elabProblems, source))
          } else {
            val maybeAst = module.readStable(solver, resultCell).map(ast => substituteSolutions(ast)(using module, solver))
            val maybeTy = module.readStable(solver, tyCell).map(t => substituteSolutions(t)(using module, solver))
            maybeAst match {
              case Some(ast) => Right((ast, maybeTy))
              case None      => Left(Seq("Elaboration did not produce a result"))
            }
          }
      }
    }
  }

  private def printLines(lines: Seq[String], toStderr: Boolean): F[Unit] = {
    lines.foldLeft(Runner.pure[F, Unit](()))((acc, line) => acc.flatMap(_ => IO.println(line, toStderr)))
  }

  private def formatAst(ast: AST, ty: Option[AST]): String = {
    val astDoc = ast.toDoc.toString
    ty match {
      case Some(tpe) =>
        val tyDoc = tpe.toDoc.toString
        s"Type: $tyDoc\nAST:\n$astDoc"
      case None =>
        s"AST:\n$astDoc"
    }
  }

  private def showAnalysis(result: Either[Seq[String], (AST, Option[AST])], output: Option[String]): F[Unit] = {
    result match {
      case Left(errors) =>
        printLines(errors, toStderr = true)
      case Right((ast, ty)) =>
        val rendered = formatAst(ast, ty)
        output match {
          case Some(pathStr) =>
            val path = io.pathOps.of(pathStr)
            IO.writeString(path, rendered, writeMode = WriteMode.Overwrite)
              .flatMap(_ => IO.println(s"Wrote elaborated AST to '${io.pathOps.asString(path)}'."))
          case None =>
            IO.println(rendered)
        }
    }
  }

  private def runRepl(): F[Unit] = {
    Terminal.runTerminal(TerminalInit.Default) {
      def loop(using term: InTerminal[F]): F[Unit] = {
        InTerminal.readline(ReplInfo).flatMap {
          case LineRead(line) =>
            val trimmed = line.trim
            if (trimmed == ":quit" || trimmed == ":q") IO.println("Goodbye.")
            else {
              showAnalysis(analyze(Source(FileNameAndContent("<stdin>", line))), None)
                .flatMap(_ => loop)
            }
          case StatusError(message) =>
            IO.println(s"Input error: $message", toStderr = true).flatMap(_ => loop)
          case UserInterrupted =>
            IO.println("Interrupted.")
          case EndOfFile =>
            IO.println("Goodbye.")
        }
      }

      loop
    }
  }

  private def runFile(pathStr: String): F[Unit] = {
    val path = io.pathOps.of(pathStr)
    for {
      exists <- IO.exists(path)
      _ <-
        if !exists then IO.println(s"Input file '$pathStr' does not exist.", toStderr = true)
        else {
          for {
            content <- IO.readString(path)
            _ <- showAnalysis(analyze(Source(FileNameAndContent(pathStr, content))), None)
          } yield ()
        }
    } yield ()
  }

  private def compileFile(input: String, output: Option[String]): F[Unit] = {
    val path = io.pathOps.of(input)
    for {
      exists <- IO.exists(path)
      _ <-
        if !exists then IO.println(s"Input file '$input' does not exist.", toStderr = true)
        else {
          for {
            content <- IO.readString(path)
            _ <- showAnalysis(analyze(Source(FileNameAndContent(input, content))), output)
          } yield ()
        }
    } yield ()
  }

  def run(config: Config): F[Unit] = config match {
    case Config.Version =>
      IO.println(s"Chester version: ${VersionInfo.current}")
    case Config.Help =>
      IO.println(Main.usage)
    case Config.Run(None) =>
      runRepl()
    case Config.Run(Some(path)) =>
      runFile(path)
    case Config.Compile(input, output) =>
      compileFile(input, output)
  }
}

object CLI {
  def run[F[_]](config: Config)(using
      runner: Runner[F],
      terminal: Terminal[F],
      io: IO[F],
      spawn: Spawn[F]
  ): Unit = {
    Spawn.spawn {
      (new CLI[F]).run(config)
    }
  }
}
