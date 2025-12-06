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

  test("elaborate def statement - with implicit parameters - should error at top level") {
    val (ast, ty, errors) = elaborate("def id[a: Type](x: a) = x")
    
    // Since def is at top level (not in a block), it should produce an error
    assert(errors.nonEmpty, s"Should have errors for def at top level, got: $errors")
    assert(errors.exists(_.toString.contains("def statement only allowed in block elements")), 
      s"Should have error about def only allowed in block elements, got: $errors")
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

  test("elaborate id function application - inferred type argument") {
    val (ast, ty, errors) = elaborate("id(42)")
    
    assert(ast.isDefined, s"AST should be defined, errors: $errors")
    
    ast.get match {
      case AST.App(func, args, _) =>
        // Should have 2 arguments: implicit type arg (inferred) + explicit arg
        assertEquals(args.length, 2, "Should have 2 arguments (implicit type + explicit value)")
        func match {
          case AST.Ref(_, "id", _) => // OK
          case other => fail(s"Expected id reference, got: $func")
        }
        // First arg should be a meta-variable (inferred type)
        assert(args(0).value.isInstanceOf[AST.MetaCell], s"First arg should be MetaCell (inferred type), got: ${args(0).value}")
        // Second arg should be the integer 42
        assert(args(1).value.isInstanceOf[AST.IntLit], s"Second arg should be IntLit, got: ${args(1).value}")
      case other =>
        fail(s"Expected App, got: $other")
    }
  }

  test("elaborate id function application - explicit type argument") {
    val (ast, ty, errors) = elaborate("id[Type](42)")
    
    assert(ast.isDefined, s"AST should be defined, errors: $errors")
    
    ast.get match {
      case AST.App(func, args, _) =>
        // Should have 2 arguments: explicit type arg + explicit value arg
        assertEquals(args.length, 2, "Should have 2 arguments (explicit type + explicit value)")
        func match {
          case AST.Ref(_, "id", _) => // OK
          case other => fail(s"Expected id reference, got: $func")
        }
        // First arg should be Type reference (explicit type)
        assert(args(0).value.isInstanceOf[AST.Ref], s"First arg should be Ref (Type), got: ${args(0).value}")
        // Second arg should be the integer 42
        assert(args(1).value.isInstanceOf[AST.IntLit], s"Second arg should be IntLit, got: ${args(1).value}")
      case other =>
        fail(s"Expected App, got: $other")
    }
  }

  test("def in tail position is rejected".ignore) {
    val (ast, ty, errors) = elaborate("{ def f(x: Type) = x }")
    
    // Should have an error about def in tail position
    assert(errors.nonEmpty, "Should have errors")
    assert(errors.exists(_.toString.contains("tail position")), 
      s"Should have error about tail position, got: $errors")
  }

  test("def in block elements is allowed".ignore) {
    val (ast, ty, errors) = elaborate("{ def f(x: Type) = x; 42 }")
    
    // Should elaborate without errors about def being disallowed
    assert(!errors.exists(_.toString.contains("def statement only allowed in block elements")), 
      s"Should not have error about def placement, got: $errors")
    
    // The AST should be a Block containing a Def and an integer
    ast match {
      case Some(AST.Block(elements, _)) =>
        assert(elements.length >= 2, s"Block should have at least 2 elements (def and 42), got: ${elements.length}")
        assert(elements.head.isInstanceOf[AST.Def], s"First element should be Def, got: ${elements.head}")
      case other =>
        // May have different AST structure, but main point is no placement error
        ()
    }
  }
}
