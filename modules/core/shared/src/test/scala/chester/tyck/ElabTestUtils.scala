package chester.tyck

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, CST}
import chester.error.VectorReporter
import chester.reader.{CharReader, FileNameAndContent, ParseError, Parser, Source, Tokenizer}
import chester.utils.elab.*

/** Shared helpers for elaboration in tests to avoid duplication. */
object ElabTestUtils:
  given ExecutionContext = ExecutionContext.global
  val defaultTimeout: FiniteDuration = 10.seconds

  def runAsync(body: => Unit): Future[Unit] = Future(body)

  def elaborateExpr(input: String, ensureCoreType: Boolean = false): (Option[AST], Option[AST], Vector[ElabProblem]) =
    val source = Source(FileNameAndContent("test.chester", input))
    elaborateCommon(source, parseAsFile = false, ensureCoreType)

  def elaborateFile(input: String, ensureCoreType: Boolean = false): (Option[AST], Option[AST], Vector[ElabProblem]) =
    val source = Source(FileNameAndContent("test.chester", input))
    elaborateCommon(source, parseAsFile = true, ensureCoreType)

  def elaborateModule(inputs: Seq[String], ensureCoreType: Boolean = false): (Seq[Option[AST]], Seq[Option[AST]], Vector[ElabProblem]) =
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val csts = inputs.map { in =>
      val source = Source(FileNameAndContent("test.chester", in))
      val parsed = for
        chars <- CharReader.read(source)
        tokens <- Tokenizer.tokenize(chars)
      yield Parser.parseFile(tokens)
      parsed.toOption.get
    }

    given module: ProceduralSolverModule.type = ProceduralSolverModule
    val results = Elaborator.elaborateModule(csts, elabReporter)
    val asts = results.map(_._1)
    val tys = results.map(_._2)

    if ensureCoreType && elabReporter.getReports.isEmpty then
      asts.flatten.foreach { ast =>
        assert(CoreTypeChecker.typeChecks(ast), s"CoreTypeChecker rejected elaborated AST in elaborateModule: $ast")
      }

    (asts, tys, elabReporter.getReports)

  private def elaborateCommon(source: Source, parseAsFile: Boolean, ensureCoreType: Boolean)(using
      ExecutionContext
  ): (Option[AST], Option[AST], Vector[ElabProblem]) =
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val result = for
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    yield
      val parsed: CST =
        if parseAsFile then Parser.parseFile(tokens)
        else Parser.parse(tokens).cst

      given module: ProceduralSolverModule.type = ProceduralSolverModule

      case class ElabHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ElabConstraint, M]:
        override def getHandler(constraint: ElabConstraint): Option[Handler[ElabConstraint]] =
          Some(new ElabHandler)

      val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))
      val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
      val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

      module.addConstraint(solver, ElabConstraint.Infer(parsed, resultCell, typeCell, ElabContext(Map.empty, Map.empty, reporter = elabReporter)))
      module.run(solver)

      val result = module.readStable(solver, resultCell)
      val ty = module.readStable(solver, typeCell)
      val zonkedResult = result.map(r => substituteSolutions(r)(using module, solver))
      val zonkedTy = ty.map(t => substituteSolutions(t)(using module, solver))

      if ensureCoreType && elabReporter.getReports.isEmpty then
        zonkedResult.foreach(ast => assert(CoreTypeChecker.typeChecks(ast), s"CoreTypeChecker rejected elaborated AST: $ast"))

      (zonkedResult, zonkedTy)

    result match
      case Right((ast, ty)) => (ast, ty, elabReporter.getReports)
      case Left(err)    => (None, None, Vector(ElabProblem.UnboundVariable(err.toString, None)))
