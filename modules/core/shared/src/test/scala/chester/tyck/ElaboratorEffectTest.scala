package chester.tyck

import scala.language.experimental.genericNumberLiterals

import munit.FunSuite
import cats.data.NonEmptyVector
import chester.core.{AST, CST}
import chester.error.Reporter
import chester.uniqid.Uniqid
import chester.utils.elab.*

class ElaboratorEffectTest extends FunSuite {

  private class VectorReporter[P] extends Reporter[P]:
    private val buf = scala.collection.mutable.ArrayBuffer.empty[P]
    def report(problem: P): Unit = buf += problem
    def getReports: Vector[P] = buf.toVector

  private case class TestHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ElabConstraint, M]:
    def getHandler(constraint: ElabConstraint): Option[Handler[ElabConstraint]] = Some(new ElabHandler)

  private def emptyTuple(span: Option[chester.error.Span]) = CST.Tuple(Vector.empty, span)

  test("propagates user defined effect rows through calls") {
    given module: ProceduralSolverModule.type = ProceduralSolverModule
    val solver = module.makeSolver[ElabConstraint](TestHandlerConf(module))
    val reporter = new VectorReporter[ElabProblem]()

    // Pretend there is a previously defined effectful function: def foo(): Integer / [magic]
    val fooId = Uniqid.make[AST]
    val fooTyCell = module.newOnceCell[ElabConstraint, AST](solver)
    module.fill(solver, fooTyCell, AST.Pi(Vector.empty, AST.IntegerType(None), Vector("magic"), None))

    // Pre-register the target def so we can observe its inferred type cell
    val barId = Uniqid.make[AST]
    val barTyCell = module.newOnceCell[ElabConstraint, AST](solver)

    val ctx = ElabContext(
      bindings = Map("foo" -> fooId, "bar" -> barId),
      types = Map(fooId -> fooTyCell, barId -> barTyCell),
      reporter = reporter
    )

    val fooCall = CST.SeqOf(NonEmptyVector.fromVectorUnsafe(Vector(CST.Symbol("foo", None), emptyTuple(None))), None)
    val defBarElems = Vector(CST.Symbol("def", None), CST.Symbol("bar", None), CST.Symbol("=", None), fooCall)
    val defBar = CST.SeqOf(NonEmptyVector.fromVectorUnsafe(defBarElems), None)
    val block = CST.Block(Vector(defBar), None, None)

    val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
    val typeCell = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Infer(block, resultCell, typeCell, ctx))

    module.run(solver)

    val barTy = module.readStable(solver, barTyCell).getOrElse(fail("bar type missing"))
    barTy match
      case AST.Pi(_, resTy, effects, _) =>
        assertEquals(effects.toSet, Set("magic"), clue = s"expected magic effect, got $effects; resultTy=$resTy")
      case other =>
        fail(s"expected Pi type for bar, got $other")

    assertEquals(reporter.getReports.size, 0)
  }

  test("explicit annotation must include required effects") {
    given module: ProceduralSolverModule.type = ProceduralSolverModule
    val solver = module.makeSolver[ElabConstraint](TestHandlerConf(module))
    val reporter = new VectorReporter[ElabProblem]()

    val fooId = Uniqid.make[AST]
    val fooTyCell = module.newOnceCell[ElabConstraint, AST](solver)
    module.fill(solver, fooTyCell, AST.Pi(Vector.empty, AST.IntegerType(None), Vector("magic"), None))

    val handledId = Uniqid.make[AST]
    val handledTyCell = module.newOnceCell[ElabConstraint, AST](solver)

    val ctx = ElabContext(
      bindings = Map("foo" -> fooId, "handled" -> handledId),
      types = Map(fooId -> fooTyCell, handledId -> handledTyCell),
      reporter = reporter
    )

    val fooCall = CST.SeqOf(NonEmptyVector.fromVectorUnsafe(Vector(CST.Symbol("foo", None), emptyTuple(None))), None)
    val handledElems = Vector(
      CST.Symbol("def", None),
      CST.Symbol("handled", None),
      CST.Symbol(":", None),
      CST.Symbol("Integer", None),
      CST.Symbol("/", None),
      CST.ListLiteral(Vector(CST.Symbol("magic", None)), None),
      CST.Symbol("=", None),
      fooCall
    )
    val handledDef = CST.SeqOf(NonEmptyVector.fromVectorUnsafe(handledElems), None)
    val block = CST.Block(Vector(handledDef), None, None)

    val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
    val typeCell = module.newOnceCell[ElabConstraint, AST](solver)
    module.addConstraint(solver, ElabConstraint.Infer(block, resultCell, typeCell, ctx))

    module.run(solver)

    val handledTy = module.readStable(solver, handledTyCell).getOrElse(fail("handled type missing"))
    handledTy match
      case AST.Pi(_, _, effects, _) =>
        assertEquals(effects.toSet, Set("magic"), clue = s"expected magic effect to be required, got $effects")
      case other =>
        fail(s"expected Pi type for handled, got $other")

    assertEquals(reporter.getReports.size, 0)
  }
}
