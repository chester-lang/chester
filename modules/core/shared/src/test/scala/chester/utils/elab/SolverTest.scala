package chester.utils.elab

import chester.utils.cell.*

class SolverTest extends munit.FunSuite {
  
  // Test with ProceduralSolver
  test("ProceduralSolver - basic cell operations") {
    testBasicCellOperations(ProceduralSolver)
  }
  
  test("ProceduralSolver - constraint solving") {
    testConstraintSolving(ProceduralSolver)
  }
  
  test("ProceduralSolver - type member access") {
    testTypeMembers(ProceduralSolver)
  }
  
  // Common test logic that works with any SolverOps
  def testBasicCellOperations(factory: SolverFactory): Unit = {
    given ops: SolverOps = factory(TestHandlerConf())
    
    // Test OnceCell
    val onceCell = ops.newOnceCell[Int]()
    assert(ops.noStableValue(onceCell))
    ops.fill(onceCell, 42)
    assert(ops.hasStableValue(onceCell))
    assertEquals(ops.readStable(onceCell), Some(42))
    
    // Test MutableCell
    val mutableCell = ops.newMutableCell[String](Some("initial"))
    assertEquals(ops.readStable(mutableCell), Some("initial"))
    ops.fill(mutableCell, "updated")
    assertEquals(ops.readStable(mutableCell), Some("updated"))
    
    // Test LiteralCell
    val literalCell = ops.newLiteralCell(100)
    assert(ops.hasStableValue(literalCell))
    assertEquals(ops.readStable(literalCell), Some(100))
  }
  
  def testConstraintSolving(factory: SolverFactory): Unit = {
    given ops: SolverOps = factory(TestHandlerConf())
    
    val cell1 = ops.newOnceCell[Int]()
    val cell2 = ops.newOnceCell[Int]()
    val resultCell = ops.newOnceCell[Int]()
    
    // Add constraint that depends on cell1 and cell2
    ops.addConstraint(TestSumConstraint(cell1, cell2, resultCell))
    
    // Fill inputs
    ops.fill(cell1, 10)
    ops.fill(cell2, 20)
    
    // Run solver
    ops.run()
    
    // Check result
    assertEquals(ops.readStable(resultCell), Some(30))
  }
  
  def testTypeMembers(factory: SolverFactory): Unit = {
    given solver: SolverOps = factory(TestHandlerConf())
    
    // Test that we can use solver.CellR, solver.OnceCell, etc.
    val cell1: solver.OnceCell[Int] = solver.newOnceCell[Int]()
    val cell2: solver.CellR[Int] = cell1
    val cell3: solver.CellW[Int] = cell1
    
    // Test variance: CellR is covariant
    val numCell: solver.CellR[Int] = solver.newOnceCell[Int]()
    val anyCell: solver.CellR[Any] = numCell  // Should compile due to covariance
    
    assert(solver.noStableValue(cell1))
  }
}

// Test constraint that sums two cells
case class TestSumConstraint(
  cell1: Cell[Int, Nothing, CellContent[Int, Nothing]],
  cell2: Cell[Int, Nothing, CellContent[Int, Nothing]],
  result: Cell[Int, Int, CellContent[Int, Int]]
) extends Constraint {
  override val kind: ConstraintKind = TestSumConstraintKind
}

object TestSumConstraintKind extends ConstraintKind {
  override type Of = TestSumConstraint
}

// Test handler configuration
case class TestHandlerConf() extends HandlerConf[Unit] {
  override def getHandler(kind: ConstraintKind): Option[Handler[Unit, kind.type]] = {
    if (kind == TestSumConstraintKind) {
      Some(TestSumHandler.asInstanceOf[Handler[Unit, kind.type]])
    } else {
      None
    }
  }
}

object TestSumHandler extends Handler[Unit, TestSumConstraintKind.type](TestSumConstraintKind) {
  override def run(constraint: TestSumConstraint)(using ops: SolverOps, ctx: Unit): Result = {
    val v1 = ops.readStable(constraint.cell1)
    val v2 = ops.readStable(constraint.cell2)
    
    (v1, v2) match {
      case (Some(a), Some(b)) =>
        ops.fill(constraint.result, a + b)
        Result.Done
      case _ =>
        Result.Waiting(constraint.cell1, constraint.cell2)
    }
  }
  
  override def canDefaulting(level: DefaultingLevel): Boolean = false
  
  override def defaulting(constraint: TestSumConstraint, level: DefaultingLevel)(using ops: SolverOps, ctx: Unit): Boolean = false
}
