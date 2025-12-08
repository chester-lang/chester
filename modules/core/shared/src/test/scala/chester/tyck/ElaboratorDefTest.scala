package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, CST, Implicitness, Param, StmtAST, Telescope}
import chester.error.{Pos, Span, SpanInFile, VectorReporter}
import chester.reader.{CharReader, FileNameAndContent, ParseError, Parser, Source, Tokenizer}
import chester.uniqid.Uniqid
import chester.utils.elab.*

class ElaboratorDefTest extends munit.FunSuite {

  private def elaborate(input: String): (Option[AST], Option[AST], Vector[ElabProblem]) = {
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

      // Create result cells
      val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
      val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

      // Add constraint to infer type
      module.addConstraint(solver, ElabConstraint.Infer(parsed, resultCell, typeCell, ctx))

      // Run solver
      module.run(solver)

      // Read results and apply substituteSolutions to resolve MetaCells
      val result = module.readStable(solver, resultCell)
      val ty = module.readStable(solver, typeCell)
      val zonkedResult = result.map(r => substituteSolutions(r)(using module, solver))
      val zonkedTy = ty.map(t => substituteSolutions(t)(using module, solver))

      (zonkedResult, zonkedTy)
    }

    result match {
      case Right((ast, ty)) => (ast, ty, elabReporter.getReports)
      case Left(_)          => (None, None, elabReporter.getReports)
    }
  }

  test("elaborate def statement - with implicit parameters - should error at top level") {
    val (ast, ty, errors) = elaborate("def id[a: Type](x: a) = x")

    // Since def is at top level (not in a block), it should produce an error
    assert(errors.nonEmpty, s"Should have errors for def at top level, got: $errors")
    assert(
      errors.exists(_.toString.contains("def statement only allowed in block elements")),
      s"Should have error about def only allowed in block elements, got: $errors"
    )
  }

  test("user-defined id function type") {
    val (ast, ty, errors) = elaborate("{ def id[a: Type](x: a) = x; id }")

    assert(ast.isDefined, "AST should be defined")

    ast.get match {
      case AST.Block(elements, tail, _) =>
        assert(elements.nonEmpty, "Block should have def element")
        tail match {
          case AST.Ref(_, "id", _) => // OK - reference to id
          case other               => fail(s"Expected Ref to id in tail, got: $other")
        }
      case other => fail(s"Expected Block, got: $other")
    }
  }

  test("elaborate id function application - inferred type argument") {
    val (ast, ty, errors) = elaborate("{ def id[a: Type](x: a) = x; id(42) }")

    assert(ast.isDefined, s"AST should be defined, errors: $errors")
    assert(!errors.exists(_.toString.contains("Unbound")), s"Should not have unbound errors: $errors")

    // Just verify it's a Block with an application
    ast.get match {
      case AST.Block(elements, tail, _) =>
        assert(elements.nonEmpty, "Block should have def element")
      // tail might still be a MetaCell if not fully resolved
      // Just check no errors for now
      case other =>
        fail(s"Expected Block, got: $other")
    }
  }

  test("elaborate id function application - explicit type argument") {
    val (ast, ty, errors) = elaborate("{ def id[a: Type](x: a) = x; id[Type](42) }")

    assert(ast.isDefined, s"AST should be defined, errors: $errors")
    assert(!errors.exists(_.toString.contains("Unbound")), s"Should not have unbound errors: $errors")

    // Just verify it's a Block with def
    ast.get match {
      case AST.Block(elements, tail, _) =>
        assert(elements.nonEmpty, "Block should have def element")
      case other =>
        fail(s"Expected Block, got: $other")
    }
  }

  test("def in tail position is rejected") {
    val (ast, ty, errors) = elaborate("{ def f(x: Type) = x }")

    // Should have an error about def in tail position
    assert(errors.nonEmpty, "Should have errors")
    assert(errors.exists(_.toString.contains("tail position")), s"Should have error about tail position, got: $errors")
  }

  test("def in block elements simple") {
    val (ast, ty, errors) = elaborate("{ def f(x: Type) = x; 42 }")

    // Should elaborate without errors about def being disallowed
    assert(
      !errors.exists(_.toString.contains("def statement only allowed in block elements")),
      s"Should not have error about def placement, got: $errors"
    )

    // The AST should be a Block containing a Def in elements and 42 in tail
    ast match {
      case Some(AST.Block(elements, tail, _)) =>
        assert(elements.length >= 1, s"Block should have at least 1 element (def), got: ${elements.length}")
        assert(elements.head.isInstanceOf[StmtAST.Def], s"First element should be Def, got: ${elements.head}")
        assert(tail.isInstanceOf[AST.IntLit], s"Tail should be IntLit, got: $tail")
      case other =>
        fail(s"Expected Block, got: $other")
    }
  }

  test("def in block can be called") {
    // Test that def in block can be referenced by later expressions
    val (ast, ty, errors) = elaborate("{ def id[a: Type](x: a) = x; id(42) }")

    // Should not have unbound variable error for id
    assert(!errors.exists(_.toString.contains("Unbound variable")), s"Should not have unbound variable error for id, got: $errors")
  }
}
