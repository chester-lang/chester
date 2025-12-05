package chester.utils.elab

import chester.utils.cell.*

class SolverTest extends munit.FunSuite {
  
  // Test with ProceduralSolver
  test("ProceduralSolver - basic cell operations") {
    testBasicCellOperations(ProceduralSolverModule)
  }
  
  test("ProceduralSolver - constraint solving") {
    testConstraintSolving(ProceduralSolverModule)
  }
  
  test("ProceduralSolver - type member access") {
    testTypeMembers(ProceduralSolverModule)
  }
  
  // Common test logic that works with any SolverModule
  def testBasicCellOperations(module: SolverModule): Unit = {
    val solver = module.makeSolver(TestHandlerConf(module))
    
    // Test OnceCell
    val onceCell = module.newOnceCell[Int](solver)
    assert(module.noStableValue(solver, onceCell))
    module.fill(solver, onceCell.asInstanceOf[module.CellW[Int]], 42)
    assert(module.hasStableValue(solver, onceCell))
    assertEquals(module.readStable(solver, onceCell.asInstanceOf[module.CellR[Int]]), Some(42))
    
    // Test MutableCell
    val mutableCell = module.newMutableCell[String](solver, Some("initial"))
    assertEquals(module.readStable(solver, mutableCell.asInstanceOf[module.CellR[String]]), Some("initial"))
    module.fill(solver, mutableCell.asInstanceOf[module.CellW[String]], "updated")
    assertEquals(module.readStable(solver, mutableCell.asInstanceOf[module.CellR[String]]), Some("updated"))
    
    // Test LiteralCell
    val literalCell = module.newLiteralCell(solver, 100)
    assert(module.hasStableValue(solver, literalCell))
    assertEquals(module.readStable(solver, literalCell), Some(100))
  }
  
  def testConstraintSolving(module: SolverModule): Unit = {
    val solver = module.makeSolver(TestHandlerConf(module))
    
    val cell1 = module.newOnceCell[Int](solver)
    val cell2 = module.newOnceCell[Int](solver)
    val resultCell = module.newOnceCell[Int](solver)
    
    // Add constraint that depends on cell1 and cell2
    module.addConstraint(solver, TestSumConstraint(cell1.asInstanceOf[Cell[Int, Nothing, CellContent[Int, Nothing]]], 
                                                    cell2.asInstanceOf[Cell[Int, Nothing, CellContent[Int, Nothing]]], 
                                                    resultCell.asInstanceOf[Cell[Int, Int, CellContent[Int, Int]]]))
    
    // Fill inputs
    module.fill(solver, cell1.asInstanceOf[module.CellW[Int]], 10)
    module.fill(solver, cell2.asInstanceOf[module.CellW[Int]], 20)
    
    // Run solver
    module.run(solver)
    
    // Check result
    assertEquals(module.readStable(solver, resultCell.asInstanceOf[module.CellR[Int]]), Some(30))
  }
  
  def testTypeMembers(module: SolverModule): Unit = {
    val solver = module.makeSolver(TestHandlerConf(module))
    
    // Test that we can use module.CellR, module.OnceCell, etc.
    val cell1: module.OnceCell[Int] = module.newOnceCell[Int](solver)
    val cell2: module.CellR[Int] = cell1.asInstanceOf[module.CellR[Int]]
    val cell3: module.CellRW[Int] = cell1.asInstanceOf[module.CellRW[Int]]
    
    // Test variance: CellR is covariant
    val numCell: module.CellR[Int] = module.newOnceCell[Int](solver).asInstanceOf[module.CellR[Int]]
    val anyCell: module.CellR[Any] = numCell  // Should compile due to covariance
    
    assert(module.noStableValue(solver, cell1))
  }
}

// Test constraint that sums two cells
case class TestSumConstraint(
  cell1: Cell[Int, Nothing, CellContent[Int, Nothing]],
  cell2: Cell[Int, Nothing, CellContent[Int, Nothing]],
  result: Cell[Int, Int, CellContent[Int, Int]]
) extends Constraint(TestSumConstraintKind)

object TestSumConstraintKind extends Kind {
  override type Of = TestSumConstraint
}

// Test handler configuration
case class TestHandlerConf[M <: SolverModule](module: M) extends HandlerConf[M] {
  override def getHandler(kind: Kind): Option[Handler[? <: Kind]] = {
    if (kind == TestSumConstraintKind) {
      Some(TestSumHandler)
    } else {
      None
    }
  }
}

object TestSumHandler extends Handler[TestSumConstraintKind.type](TestSumConstraintKind) {
  override def run[M <: SolverModule](constraint: TestSumConstraint)(using module: M, solver: module.Solver): Result = {
    val v1 = module.readStable(solver, constraint.cell1.asInstanceOf[module.CellR[Int]])
    val v2 = module.readStable(solver, constraint.cell2.asInstanceOf[module.CellR[Int]])
    
    (v1, v2) match {
      case (Some(a), Some(b)) =>
        module.fill(solver, constraint.result.asInstanceOf[module.CellW[Int]], a + b)
        Result.Done
      case _ =>
        Result.Waiting(constraint.cell1, constraint.cell2)
    }
  }
  
  override def canDefaulting(level: DefaultingLevel): Boolean = false
  
  override def defaulting[M <: SolverModule](constraint: TestSumConstraint, level: DefaultingLevel)(using module: M, solver: module.Solver): Boolean = false
}
