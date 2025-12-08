package chester.tyck

import scala.collection.mutable
import scala.language.experimental.genericNumberLiterals

import munit.FunSuite
import chester.core.{AST, CST}
import chester.error.{Problem, Reporter}
import chester.utils.elab.*

class ElaboratorTest extends FunSuite {

  class VectorReporter[P <: Problem] extends Reporter[P]:
    private val problems = mutable.ArrayBuffer[P]()
    def report(problem: P): Unit = problems += problem
    def getProblems: Vector[P] = problems.toVector

  case class ElabHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ElabConstraint, M]:
    override def getHandler(constraint: ElabConstraint): Option[Handler[ElabConstraint]] =
      Some(new ElabHandler)

  test("elaborate string literal directly") {
    val reporter = new VectorReporter[ElabProblem]()
    val ctx = ElabContext(bindings = Map.empty, types = Map.empty, reporter = reporter)

    given module: ProceduralSolverModule.type = ProceduralSolverModule
    val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))

    val cst = CST.StringLiteral("a", None)
    val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
    val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

    // Add single Infer constraint
    module.addConstraint(solver, ElabConstraint.Infer(cst, resultCell, typeCell, ctx))

    // Run once
    module.run(solver)

    // Check results
    val result = module.readStable(solver, resultCell)
    val ty = module.readStable(solver, typeCell)

    assert(result.isDefined, "Result should be filled")
    assert(ty.isDefined, "Type should be filled")

    result.get match {
      case AST.StringLit(value, _) =>
        assertEquals(value, "a", "String value should be 'a'")
      case other =>
        fail(s"Expected StringLit, got $other")
    }

    assertEquals(reporter.getProblems.size, 0, "Should have no errors")
  }

  test("CST structure - string literal") {
    val cst = CST.StringLiteral("a", None)

    cst match {
      case CST.StringLiteral(value, _) =>
        assertEquals(value, "a", "String value should be 'a'")
      case other =>
        fail(s"Expected StringLiteral, got $other")
    }
  }

  test("elaborate block with string literal tail") {
    val reporter = new VectorReporter[ElabProblem]()
    val ctx = ElabContext(bindings = Map.empty, types = Map.empty, reporter = reporter)

    given module: ProceduralSolverModule.type = ProceduralSolverModule
    val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))

    val cst = CST.Block(Vector.empty, Some(CST.StringLiteral("a", None)), None)
    val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
    val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

    module.addConstraint(solver, ElabConstraint.Infer(cst, resultCell, typeCell, ctx))

    // Run solver
    module.run(solver)

    // Check results and resolve MetaCells
    val result = module.readStable(solver, resultCell).map(r => substituteSolutions(r)(using module, solver))
    val ty = module.readStable(solver, typeCell).map(t => substituteSolutions(t)(using module, solver))

    assert(result.isDefined, s"Result should be filled, but got: $result")
    assert(ty.isDefined, s"Type should be filled, but got: $ty")

    result.get match {
      case AST.Block(elements, tail, _) =>
        assertEquals(elements.size, 0, "Block should have 0 elements")
        tail match {
          case AST.StringLit(value, _) =>
            assertEquals(value, "a", "Block tail should be 'a'")
          case other =>
            fail(s"Expected StringLit in block tail, got $other")
        }
      case other =>
        fail(s"Expected Block, got $other")
    }

    assertEquals(reporter.getProblems.size, 0, "Should have no errors")
  }

  test("CST structure - block with string literal tail") {
    val cst = CST.Block(Vector.empty, Some(CST.StringLiteral("a", None)), None)

    cst match {
      case CST.Block(elements, tail, _) =>
        assertEquals(elements.size, 0, "Block should have no body elements")
        tail match {
          case Some(CST.StringLiteral(value, _)) =>
            assertEquals(value, "a", "Block tail should be 'a'")
          case other =>
            fail(s"Expected StringLiteral in tail, got $other")
        }
      case other =>
        fail(s"Expected Block, got $other")
    }
  }
}
