package chester.tyck

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.experimental.genericNumberLiterals

import munit.FunSuite
import chester.core.AST
import chester.error.VectorReporter
import chester.reader.{CharReader, FileNameAndContent, ParseError, Parser, Source, Tokenizer}
import chester.utils.elab.*

class ElaboratorEffectTest extends FunSuite {
  private def runAsync(body: => Unit): Future[Unit] = Future(body)

  private def elaborate(input: String): (Option[AST], Option[AST], Vector[ElabProblem]) =
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val source = Source(FileNameAndContent("effect.chester", input))

    val result = for
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    yield
      val parsed = Parser.parseFile(tokens)

      given module: ProceduralSolverModule.type = ProceduralSolverModule
      val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))

      val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
      val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

      module.addConstraint(solver, ElabConstraint.Infer(parsed, resultCell, typeCell, ElabContext(Map.empty, Map.empty, reporter = elabReporter)))
      module.run(solver)

      val result = module.readStable(solver, resultCell)
      val ty = module.readStable(solver, typeCell)
      val zonkedResult = result.map(r => substituteSolutions(r)(using module, solver))
      val zonkedTy = ty.map(t => substituteSolutions(t)(using module, solver))
      (zonkedResult, zonkedTy)

    result match
      case Right((ast, ty)) => (ast, ty, elabReporter.getReports)
      case Left(err)        => (None, None, Vector(ElabProblem.UnboundVariable(err.toString, None)))

  test("propagates user defined effect rows through calls") {
    runAsync {
      val code =
        """{
          |  effect magic;
          |  def foo(): Integer / [magic] = 1;
          |  def bar(): Integer = foo();
          |  bar
          |}""".stripMargin

      val (_, barTy, errors) = elaborate(code)
      assert(errors.isEmpty, s"Expected no errors, got: $errors")

      barTy match
        case Some(AST.Pi(_, _, effects, _)) =>
          assertEquals(effects.toSet, Set("magic"), clue = s"bar should propagate foo's effect, got $effects")
        case other =>
          fail(s"Expected Pi type for bar reference, got: $other")
    }
  }

  test("annotated function must include required effects") {
    runAsync {
      val code =
        """{
          |  effect magic;
          |  def foo(): Integer / [magic] = 1;
          |  def bad(): Integer / [] = foo();
          |  bad
          |}""".stripMargin

      val (_, _, errors) = elaborate(code)
      assert(errors.nonEmpty, "Expected error when annotated effects do not cover required effects")
    }
  }

  test("annotation that lists required effect type-checks") {
    runAsync {
      val code =
        """{
          |  effect magic;
          |  def foo(): Integer / [magic] = 1;
          |  def ok(): Integer / [magic] = foo();
          |  ok
          |}""".stripMargin

      val (_, okTy, errors) = elaborate(code)
      assert(errors.isEmpty, s"Expected no errors, got: $errors")

      okTy match
        case Some(AST.Pi(_, _, effects, _)) =>
          assertEquals(effects.toSet, Set("magic"), clue = s"ok should retain declared effect row, got $effects")
        case other =>
          fail(s"Expected Pi type for ok reference, got: $other")
    }
  }

  test("using undeclared effect reports an error") {
    runAsync {
      val code =
        """{
          |  def sneaky(): Integer / [ghost] = 1;
          |  sneaky
          |}""".stripMargin

      val (_, _, errors) = elaborate(code)
      assert(errors.nonEmpty, "Expected error when using undeclared effect ghost")
    }
  }
}
