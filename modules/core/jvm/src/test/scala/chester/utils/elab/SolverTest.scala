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
  
  // Test with ConcurrentSolver
  test("ConcurrentSolver - basic cell operations") {
    testBasicCellOperations(ConcurrentSolverModule)
  }
  
  test("ConcurrentSolver - constraint solving") {
    testConstraintSolving(ConcurrentSolverModule)
  }
  
  test("ConcurrentSolver - type member access") {
    testTypeMembers(ConcurrentSolverModule)
  }

  test("ProceduralSolver - bidirectional constraints") {
    testBidirectionalConstraints(ProceduralSolverModule)
  }

  test("ConcurrentSolver - bidirectional constraints") {
    testBidirectionalConstraints(ConcurrentSolverModule)
  }

  test("ProceduralSolver - complex multi-directional flow") {
    testComplexMultiDirectionalFlow(ProceduralSolverModule)
  }

  test("ConcurrentSolver - complex multi-directional flow") {
    testComplexMultiDirectionalFlow(ConcurrentSolverModule)
  }
  
  // Common test logic that works with any SolverModule
  def testBasicCellOperations(module: SolverModule): Unit = {
    import module.given
    val solver = module.makeSolver[TestSumConstraint](TestHandlerConf(module))
    
    // Test OnceCell
    val onceCell = module.newOnceCell[TestSumConstraint, Int](solver)
    assert(module.noStableValue(solver, onceCell))
    module.fill(solver, onceCell, 42)
    assert(module.hasStableValue(solver, onceCell))
    assertEquals(module.readStable(solver, onceCell), Some(42), "OnceCell value")
    
    // Test MutableCell
    val mutableCell = module.newMutableCell[TestSumConstraint, String](solver, Some("initial"))
    assertEquals(module.readStable(solver, mutableCell), Some("initial"), "MutableCell initial")
    module.fill(solver, mutableCell, "updated")
    assertEquals(module.readStable(solver, mutableCell), Some("updated"), "MutableCell updated")
    
    // Test LiteralCell
    val literalCell = module.newLiteralCell(solver, 100)
    assert(module.hasStableValue(solver, literalCell))
    assertEquals(module.readStable(solver, literalCell), Some(100), "LiteralCell value")
  }
  
  def testConstraintSolving(module: SolverModule): Unit = {
    import module.given
    val solver = module.makeSolver[TestSumConstraint](TestHandlerConf(module))
    
    val cell1 = module.newOnceCell[TestSumConstraint, Int](solver)
    val cell2 = module.newOnceCell[TestSumConstraint, Int](solver)
    val resultCell = module.newOnceCell[TestSumConstraint, Int](solver)
    
    // Add constraint that depends on cell1 and cell2
    module.addConstraint(solver, TestSumConstraint(cell1, cell2, resultCell))
    
    // Fill inputs
    module.fill(solver, cell1, 10)
    module.fill(solver, cell2, 20)
    
    // Run solver
    module.run(solver)
    
    // Check result
    assertEquals(module.readStable(solver, resultCell), Some(30), "Sum result")
  }
  
  def testTypeMembers(module: SolverModule): Unit = {
    import module.given
    val solver = module.makeSolver[TestSumConstraint](TestHandlerConf(module))
    
    // Test that we can use module.CellR, module.OnceCell, etc.
    val cell1: module.OnceCell[Int] = module.newOnceCell[TestSumConstraint, Int](solver)
    val cell2: module.CellR[Int] = cell1
    val cell3: module.CellRW[Int] = cell1
    
    // Test variance: CellR is covariant
    val numCell: module.CellR[Int] = module.newOnceCell[TestSumConstraint, Int](solver)
    val anyCell: module.CellR[Any] = numCell  // Should compile due to covariance
    
    assert(module.noStableValue(solver, cell1))
  }

  // Test bidirectional constraint: a * b = c
  // Can solve for any missing variable given the other two
  def testBidirectionalConstraints(module: SolverModule): Unit = {
    import module.given
    // Scenario 1: Given a=5, b=3, solve for c (forward: c = a*b)
    {
      val solver = module.makeSolver[ProductConstraint](ProductHandlerConf(module))
      val a = module.newOnceCell[ProductConstraint, Int](solver)
      val b = module.newOnceCell[ProductConstraint, Int](solver)
      val c = module.newOnceCell[ProductConstraint, Int](solver)

      module.addConstraint(solver, ProductConstraint(a, b, c))
      
      module.fill(solver, a, 5)
      module.fill(solver, b, 3)
      module.run(solver)
      
      assertEquals(module.readStable(solver, c), Some(15), "Forward: 5 * 3 = 15")
    }

    // Scenario 2: Given a=6, c=18, solve for b (backward: b = c/a)
    {
      val solver = module.makeSolver[ProductConstraint](ProductHandlerConf(module))
      val a = module.newOnceCell[ProductConstraint, Int](solver)
      val b = module.newOnceCell[ProductConstraint, Int](solver)
      val c = module.newOnceCell[ProductConstraint, Int](solver)

      module.addConstraint(solver, ProductConstraint(a, b, c))
      
      module.fill(solver, a, 6)
      module.fill(solver, c, 18)
      module.run(solver)
      
      assertEquals(module.readStable(solver, b), Some(3), "Backward: 18 / 6 = 3")
    }

    // Scenario 3: Given b=4, c=20, solve for a (backward: a = c/b)
    {
      val solver = module.makeSolver[ProductConstraint](ProductHandlerConf(module))
      val a = module.newOnceCell[ProductConstraint, Int](solver)
      val b = module.newOnceCell[ProductConstraint, Int](solver)
      val c = module.newOnceCell[ProductConstraint, Int](solver)

      module.addConstraint(solver, ProductConstraint(a, b, c))
      
      module.fill(solver, b, 4)
      module.fill(solver, c, 20)
      module.run(solver)
      
      assertEquals(module.readStable(solver, a), Some(5), "Backward: 20 / 4 = 5")
    }
  }

  // Test complex multi-directional flow: system of equations
  // a + b = sum1
  // b + c = sum2
  // Given a and sum1 and sum2, solve for b and c
  def testComplexMultiDirectionalFlow(module: SolverModule): Unit = {
    import module.given
    val solver = module.makeSolver[MixedConstraint](MixedHandlerConf(module))
    
    val a = module.newOnceCell[MixedConstraint, Int](solver)
    val b = module.newOnceCell[MixedConstraint, Int](solver)
    val c = module.newOnceCell[MixedConstraint, Int](solver)
    val sum1 = module.newOnceCell[MixedConstraint, Int](solver)
    val sum2 = module.newOnceCell[MixedConstraint, Int](solver)
    val product = module.newOnceCell[MixedConstraint, Int](solver)

    // Add constraints: a + b = sum1, b + c = sum2, b * c = product
    module.addConstraint(solver, SumConstraint(a, b, sum1))
    module.addConstraint(solver, SumConstraint(b, c, sum2))
    module.addConstraint(solver, ProductConstraint(b, c, product))

    // Given: a=5, sum1=12, sum2=15
    // Should solve: b=7 (from a+b=12), c=8 (from b+c=15), product=56 (from b*c)
    module.fill(solver, a, 5)
    module.fill(solver, sum1, 12)
    module.fill(solver, sum2, 15)
    
    module.run(solver)
    
    assertEquals(module.readStable(solver, b), Some(7), "b should be 7 (12-5)")
    assertEquals(module.readStable(solver, c), Some(8), "c should be 8 (15-7)")
    assertEquals(module.readStable(solver, product), Some(56), "product should be 56 (7*8)")
  }
}

// Test constraint that sums two cells - just a plain case class
case class TestSumConstraint(
  cell1: Cell[Int, Nothing, CellContent[Int, Nothing]],
  cell2: Cell[Int, Nothing, CellContent[Int, Nothing]],
  result: Cell[Int, Int, CellContent[Int, Int]]
)

// Test handler configuration
case class TestHandlerConf[M <: SolverModule](module: M) extends HandlerConf[TestSumConstraint, M] {
  override def getHandler(constraint: TestSumConstraint): Option[Handler[TestSumConstraint]] = {
    Some(TestSumHandler)
  }
}

object TestSumHandler extends Handler[TestSumConstraint] {
  override def run[M <: SolverModule](constraint: TestSumConstraint)(using module: M, solver: module.Solver[TestSumConstraint]): Result = {
    val v1 = module.readStable(solver, constraint.cell1)
    val v2 = module.readStable(solver, constraint.cell2)
    
    (v1, v2) match {
      case (Some(a), Some(b)) =>
        module.fill(solver, constraint.result, a + b)
        Result.Done
      case _ =>
        Result.Waiting(constraint.cell1, constraint.cell2)
    }
  }
  
  override def canDefaulting(level: DefaultingLevel): Boolean = false
  
  override def defaulting[M <: SolverModule](constraint: TestSumConstraint, level: DefaultingLevel)(using module: M, solver: module.Solver[TestSumConstraint]): Boolean = false
}

// Bidirectional product constraint: a * b = c
case class ProductConstraint(
  a: Cell[Int, Int, CellContent[Int, Int]],
  b: Cell[Int, Int, CellContent[Int, Int]],
  product: Cell[Int, Int, CellContent[Int, Int]]
)

case class ProductHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ProductConstraint, M] {
  override def getHandler(constraint: ProductConstraint): Option[Handler[ProductConstraint]] = Some(ProductHandler)
}

object ProductHandler extends Handler[ProductConstraint] {
  override def run[M <: SolverModule](constraint: ProductConstraint)(using module: M, solver: module.Solver[ProductConstraint]): Result = {
    val va = module.readStable(solver, constraint.a)
    val vb = module.readStable(solver, constraint.b)
    val vp = module.readStable(solver, constraint.product)
    
    (va, vb, vp) match {
      // Forward: a * b = product
      case (Some(a), Some(b), None) =>
        module.fill(solver, constraint.product, a * b)
        Result.Done
      // Backward: product / a = b
      case (Some(a), None, Some(p)) if a != 0 && p % a == 0 =>
        module.fill(solver, constraint.b, p / a)
        Result.Done
      // Backward: product / b = a
      case (None, Some(b), Some(p)) if b != 0 && p % b == 0 =>
        module.fill(solver, constraint.a, p / b)
        Result.Done
      // All filled - check consistency
      case (Some(a), Some(b), Some(p)) if a * b == p =>
        Result.Done
      case _ =>
        Result.Waiting(constraint.a, constraint.b, constraint.product)
    }
  }
  
  override def canDefaulting(level: DefaultingLevel): Boolean = false
  override def defaulting[M <: SolverModule](constraint: ProductConstraint, level: DefaultingLevel)(using module: M, solver: module.Solver[ProductConstraint]): Boolean = false
}

// Sum constraint that can solve backwards: a + b = sum
case class SumConstraint(
  a: Cell[Int, Int, CellContent[Int, Int]],
  b: Cell[Int, Int, CellContent[Int, Int]],
  sum: Cell[Int, Int, CellContent[Int, Int]]
)

// Mixed constraint type that can be either Sum or Product
type MixedConstraint = SumConstraint | ProductConstraint

case class MixedHandlerConf[M <: SolverModule](module: M) extends HandlerConf[MixedConstraint, M] {
  override def getHandler(constraint: MixedConstraint): Option[Handler[MixedConstraint]] = constraint match {
    case _: SumConstraint => Some(MixedSumHandler)
    case _: ProductConstraint => Some(MixedProductHandler)
  }
}

object MixedSumHandler extends Handler[MixedConstraint] {
  override def run[M <: SolverModule](constraint: MixedConstraint)(using module: M, solver: module.Solver[MixedConstraint]): Result = {
    constraint match {
      case c: SumConstraint =>
        val va = module.readStable(solver, c.a)
        val vb = module.readStable(solver, c.b)
        val vs = module.readStable(solver, c.sum)
        
        (va, vb, vs) match {
          // Forward: a + b = sum
          case (Some(a), Some(b), None) =>
            module.fill(solver, c.sum, a + b)
            Result.Done
          // Backward: sum - a = b
          case (Some(a), None, Some(s)) =>
            module.fill(solver, c.b, s - a)
            Result.Done
          // Backward: sum - b = a
          case (None, Some(b), Some(s)) =>
            module.fill(solver, c.a, s - b)
            Result.Done
          // All filled - check consistency
          case (Some(a), Some(b), Some(s)) if a + b == s =>
            Result.Done
          case _ =>
            Result.Waiting(c.a, c.b, c.sum)
        }
      case _ => Result.Waiting()
    }
  }
  
  override def canDefaulting(level: DefaultingLevel): Boolean = false
  override def defaulting[M <: SolverModule](constraint: MixedConstraint, level: DefaultingLevel)(using module: M, solver: module.Solver[MixedConstraint]): Boolean = false
}

object MixedProductHandler extends Handler[MixedConstraint] {
  override def run[M <: SolverModule](constraint: MixedConstraint)(using module: M, solver: module.Solver[MixedConstraint]): Result = {
    constraint match {
      case c: ProductConstraint =>
        val va = module.readStable(solver, c.a)
        val vb = module.readStable(solver, c.b)
        val vp = module.readStable(solver, c.product)
        
        (va, vb, vp) match {
          case (Some(a), Some(b), None) =>
            module.fill(solver, c.product, a * b)
            Result.Done
          case (Some(a), None, Some(p)) if a != 0 && p % a == 0 =>
            module.fill(solver, c.b, p / a)
            Result.Done
          case (None, Some(b), Some(p)) if b != 0 && p % b == 0 =>
            module.fill(solver, c.a, p / b)
            Result.Done
          case (Some(a), Some(b), Some(p)) if a * b == p =>
            Result.Done
          case _ =>
            Result.Waiting(c.a, c.b, c.product)
        }
      case _ => Result.Waiting()
    }
  }
  
  override def canDefaulting(level: DefaultingLevel): Boolean = false
  override def defaulting[M <: SolverModule](constraint: MixedConstraint, level: DefaultingLevel)(using module: M, solver: module.Solver[MixedConstraint]): Boolean = false
}
