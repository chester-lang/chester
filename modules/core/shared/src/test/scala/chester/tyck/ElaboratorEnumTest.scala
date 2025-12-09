package chester.tyck

import scala.language.experimental.genericNumberLiterals

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

import chester.core.AST
import chester.error.VectorReporter
import chester.reader.{CharReader, FileNameAndContent, ParseError, Parser, Source, Tokenizer}
import chester.utils.elab.*
import munit.FunSuite

class ElaboratorEnumTest extends FunSuite:

  override val munitTimeout: FiniteDuration = 10.seconds
  private given ExecutionContext = ExecutionContext.global

  private def runAsync(body: => Unit): Future[Unit] = Future(body)

  private def elaborate(input: String): (Option[AST], Option[AST], Vector[ElabProblem]) =
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val source = Source(FileNameAndContent("test.chester", input))

    val result = for {
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    } yield {
      val parsed = Parser.parse(tokens).cst

      // Create elaborator
      val ctx = ElabContext(bindings = Map.empty, types = Map.empty, reporter = elabReporter)

      given module: ProceduralSolverModule.type = ProceduralSolverModule

      case class ElabHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ElabConstraint, M]:
        override def getHandler(constraint: ElabConstraint): Option[Handler[ElabConstraint]] =
          Some(new ElabHandler)

      val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))

      val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
      val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

      module.addConstraint(solver, ElabConstraint.Infer(parsed, resultCell, typeCell, ctx))
      module.run(solver)

      val result = module.readStable(solver, resultCell)
      val ty = module.readStable(solver, typeCell)
      val zonkedResult = result.map(r => substituteSolutions(r)(using module, solver))
      val zonkedTy = ty.map(t => substituteSolutions(t)(using module, solver))

      (zonkedResult, zonkedTy)
    }

    result match
      case Right((ast, ty)) => (ast, ty, elabReporter.getReports)
      case Left(err) =>
        // Provide some context if parsing failed
        elabReporter.report(ElabProblem.UnboundVariable(err.toString, None))
        (None, None, elabReporter.getReports)

  test("enum case reference has enum type") {
    val code =
      """{ enum Color { case Red; case Green };
        |  Color.Red }""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborate(code)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.EnumTypeRef(_, name, _)) => assertEquals(name, "Color")
        case other                             => fail(s"Expected EnumTypeRef(Color), got $other")
    }
  }

  test("enum case with payload typechecks") {
    val code =
      """{ enum Maybe { case Some(x: Integer); case None };
        |  Maybe.Some(42) }""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborate(code)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.EnumTypeRef(_, name, _)) => assertEquals(name, "Maybe")
        case other                             => fail(s"Expected EnumTypeRef(Maybe), got $other")
    }
  }

  test("enum type reference via .t has universe type") {
    val code =
      """{ enum Result { case Ok };
        |  Result.t }""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborate(code)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.Type(AST.LevelLit(level, _), _)) => assertEquals(level, BigInt(0))
        case other                                     => fail(s"Expected Type(LevelLit(0)), got $other")
    }
  }

  test("enum statement accepts trailing semicolon") {
    val code =
      """{ enum Maybe { case Some(x: Integer); case None };
        |  42 }""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborate(code)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.IntegerType(_)) => () // tail is 42
        case other                    => fail(s"Expected Integer type, got $other")
    }
  }

  test("enum statement without semicolon is rejected") {
    val code =
      """{ enum Maybe { case Some(x: Integer); case None }
        |  42 }""".stripMargin

    runAsync {
      val (_, _, errors) = elaborate(code)
      assert(errors.exists(_.toString.contains("enum statement")), s"Expected enum placement error, got $errors")
    }
  }
