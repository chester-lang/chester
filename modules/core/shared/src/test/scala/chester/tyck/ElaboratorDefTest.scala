package chester.tyck

import scala.language.experimental.genericNumberLiterals
import chester.core.{AST, CST, Implicitness, Telescope, Param}
import chester.error.{Span, SpanInFile, Pos, VectorReporter}
import chester.reader.{Source, FileNameAndContent, CharReader, Tokenizer, Parser, ParseError}
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
      val ctx = ElabContext(Map.empty, Map.empty, reporter = elabReporter)
      
      given module: ProceduralSolverModule.type = ProceduralSolverModule
      import module.given
      
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
      
      (module.readStable(solver, resultCell), module.readStable(solver, typeCell))
    }
    
    result match {
      case Right((ast, ty)) => (ast, ty, elabReporter.getReports)
      case Left(_) => (None, None, elabReporter.getReports)
    }
  }

  test("elaborate function application - simple".ignore) {
    val (ast, ty, errors) = elaborate("f(x)")
    
    // For now, expect errors since f is unbound
    assert(ast.isDefined, "AST should be defined even with errors")
    
    ast.get match {
      case AST.App(func, args, _) =>
        assertEquals(args.length, 1, "Should have 1 argument")
      case other =>
        fail(s"Expected App, got: $other")
    }
  }

  test("elaborate function application - multiple args".ignore) {
    val (ast, ty, errors) = elaborate("f(x, y, z)")
    
    assert(ast.isDefined, "AST should be defined")
    
    ast.get match {
      case AST.App(func, args, _) =>
        assertEquals(args.length, 3, "Should have 3 arguments")
      case other =>
        fail(s"Expected App, got: $other")
    }
  }

  test("elaborate def statement - simple".ignore) {
    val (ast, ty, errors) = elaborate("def f(x: Type) = x")
    
    assert(ast.isDefined, s"AST should be defined, errors: $errors")
    
    ast.get match {
      case AST.Def(id, name, telescopes, resultTy, body, _) =>
        assertEquals(name, "f", "Function name should be f")
        assertEquals(telescopes.length, 1, "Should have 1 telescope")
        assert(telescopes(0).implicitness == Implicitness.Explicit, "Should be explicit")
        assertEquals(telescopes(0).params.length, 1, "Should have 1 parameter")
      case other =>
        fail(s"Expected Def, got: $other")
    }
  }

  test("elaborate def statement - with implicit parameters".ignore) {
    val (ast, ty, errors) = elaborate("def id[a: Type](x: a) = x")
    
    assert(ast.isDefined, s"AST should be defined, errors: $errors")
    
    ast.get match {
      case AST.Def(id, name, telescopes, resultTy, body, _) =>
        assertEquals(name, "id", "Function name should be id")
        assertEquals(telescopes.length, 2, "Should have 2 telescopes")
        assert(telescopes(0).implicitness == Implicitness.Implicit, "First telescope should be implicit")
        assert(telescopes(1).implicitness == Implicitness.Explicit, "Second telescope should be explicit")
      case other =>
        fail(s"Expected Def, got: $other")
    }
  }

  test("elaborate def statement - with result type".ignore) {
    val (ast, ty, errors) = elaborate("def f(x: Type) : Type = x")
    
    assert(ast.isDefined, s"AST should be defined, errors: $errors")
    
    ast.get match {
      case AST.Def(id, name, telescopes, Some(resultTy), body, _) =>
        assertEquals(name, "f", "Function name should be f")
        assert(resultTy.isInstanceOf[AST.Ref], "Result type should be specified")
      case other =>
        fail(s"Expected Def with result type, got: $other")
    }
  }

  test("builtin id function type") {
    val (ast, ty, errors) = elaborate("id")
    
    assert(ast.isDefined, "AST should be defined")
    assert(ty.isDefined, "Type should be defined")
    
    ty.get match {
      case AST.Pi(telescopes, resultTy, _) =>
        assertEquals(telescopes.length, 2, "id should have 2 telescopes")
        assert(telescopes(0).implicitness == Implicitness.Implicit, "First telescope should be implicit")
        assert(telescopes(1).implicitness == Implicitness.Explicit, "Second telescope should be explicit")
      case other =>
        fail(s"Expected Pi type for id, got: $other")
    }
  }

  test("elaborate id function application".ignore) {
    val (ast, ty, errors) = elaborate("id(42)")
    
    assert(ast.isDefined, s"AST should be defined, errors: $errors")
    
    ast.get match {
      case AST.App(func, args, _) =>
        assertEquals(args.length, 1, "Should have 1 argument")
        func match {
          case AST.Ref(_, "id", _) => // OK
          case other => fail(s"Expected id reference, got: $func")
        }
      case other =>
        fail(s"Expected App, got: $other")
    }
  }
}
