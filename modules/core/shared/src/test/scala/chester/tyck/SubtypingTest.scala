package chester.tyck

import scala.language.experimental.genericNumberLiterals
import chester.core.{AST, CST}
import chester.error.VectorReporter
import chester.reader.{Source, FileNameAndContent, CharReader, Tokenizer, Parser, ParseError}
import chester.utils.elab.*
import munit.FunSuite

class SubtypingTest extends FunSuite:

  def elaborate(input: String): (Option[AST], Option[AST], Vector[ElabProblem]) =
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
      
      // Read results and apply substituteSolutions to resolve MetaCells
      val result = module.readStable(solver, resultCell)
      val ty = module.readStable(solver, typeCell)
      val zonkedResult = result.map(r => substituteSolutions(r)(using module, solver))
      val zonkedTy = ty.map(t => substituteSolutions(t)(using module, solver))
      
      (zonkedResult, zonkedTy)
    }
    
    result match {
      case Right((ast, ty)) => (ast, ty, elabReporter.getReports)
      case Left(err) => (None, None, Vector(ElabProblem.UnboundVariable(err.toString, None)))
    }

  test("Any type has type Type[1]") {
    val (ast, ty, errors) = elaborate("Any")
    
    assert(errors.isEmpty, s"Should have no errors, got: $errors")
    assert(ast.isDefined, s"AST should be defined")
    
    ast.get match {
      case AST.AnyType(_) => // OK
      case other => fail(s"Expected AnyType, got: $other")
    }
    
    ty.get match {
      case AST.Universe(AST.IntLit(level, _), _) =>
        assertEquals(level, BigInt(1), "Any should have type Type[1]")
      case other => fail(s"Expected Universe[1], got: $other")
    }
  }

  test("Any type can be used in type position") {
    val (ast, ty, errors) = elaborate("{ def f(x: Any) = x; 42 }")
    
    // Should not have errors about def placement or type errors
    assert(!errors.exists(_.toString.contains("def statement only allowed")), 
      s"Should not have def placement error, got: $errors")
    assert(!errors.exists(_.toString.contains("Type mismatch")), 
      s"Should not have type errors with Any, got: $errors")
    assert(ast.isDefined, s"AST should be defined")
    
    // Just verify it's a block - the Any type should work without errors
    ast.get match {
      case AST.Block(_, _, _) => // OK
      case other => fail(s"Expected Block, got: $other")
    }
  }

  test("Any can accept any value") {
    val (ast, ty, errors) = elaborate("{ def f(x: Any) = x; f(42) }")
    
    // Should elaborate without type errors
    assert(!errors.exists(_.toString.contains("Type mismatch")), 
      s"Should not have type mismatch, got: $errors")
  }

  test("function returning Any can be called with any argument type") {
    val (ast, ty, errors) = elaborate("{ def f(x: Type): Any = x; f(Type); f(42) }")
    
    // Should elaborate - Any is the return type, accepts Type parameter
    assert(ast.isDefined, "Should elaborate")
    assert(!errors.exists(_.toString.contains("Type mismatch")), 
      s"Should not have type mismatch, got: $errors")
  }

  test("reduce function implemented for future use".ignore) {
    // Reduction is implemented but currently disabled in unification to avoid infinite loops
    // id : [a: Type](x: a) -> a
    // Future: id[Type](String) should work where String : Type
    // Future: id(x) where x is an application should reduce x first
    assert(true)
  }

  test("type check id[id(String)](\"a\") - requires reduction".ignore) {
    // TODO: This test requires reduction to work properly
    // Define id and use it with reduction
    // id : [a: Type](x: a) -> a
    // id(String) has type Type and value String (dependent types!)
    // id[id(String)]("a") means id[String]("a") after reduction
    // This should type check: "a" : String
    //
    // Currently fails because:
    // 1. Reduction is disabled in unify() to avoid infinite loops
    // 2. Need to fix the feedback loop between reduce() and constraint solving
    val (ast, ty, errors) = elaborate("""{
      def id[a: Type](x: a) = x;
      id[id(String)]("a")
    }""")
    
    // Should have no errors once reduction works
    assert(errors.isEmpty, s"Should type check with reduction, got errors: $errors")
    assert(ast.isDefined, "AST should be defined")
    assert(ty.isDefined, "Type should be defined")
  }

  test("type check id(id)(\"a\") with type String".ignore) {
    // id : [a: Type](x: a) -> a
    // id(id) applies id to itself: id[([a:Type](x:a)->a)](id) : ([a:Type](x:a)->a)
    // So id(id) returns id, and id(id)("a") should be the same as id("a")
    // The type of "a" is String, so result should have type String
    // 
    // Currently hangs because:
    // 1. Type checking id(id) requires reducing id applied to itself
    // 2. Reduction is disabled in unify() to prevent infinite loops
    // 3. Need to implement proper normalization strategy (Issue #3)
    val (ast, ty, errors) = elaborate("""{
      def id[a: Type](x: a) = x;
      id(id)("a")
    }""")
    
    assert(errors.isEmpty, s"Should have no errors, got: $errors")
    assert(ast.isDefined, "AST should be defined")
    assert(ty.isDefined, "Type should be defined")
    
    // The type should be String (the type of "a")
    ty.get match {
      case AST.StringType(_) => // OK - result type is String
      case other => fail(s"Expected String type, got: $other")
    }
  }

