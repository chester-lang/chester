package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.uniqid.Uniqid
import munit.FunSuite

class ElabRunnerTest extends FunSuite {

  test("validateCoreType returns structured problems for invalid core AST") {
    val invalidAst = AST.Ref(Uniqid.make, "missing", None)

    val errors = ElabRunner.validateCoreType(invalidAst)

    assert(errors.nonEmpty, "Expected core type validation to report an error")
    assert(
      errors.exists {
        case ElabProblem.UnboundVariable(name, _) => name == "Core type check failed"
        case _                                    => false
      },
      s"Expected core type failure to be reported structurally, got: $errors"
    )
  }

  test("validateCoreType stays quiet for a valid core AST") {
    val validAst = AST.StringLit("ok", None)

    val errors = ElabRunner.validateCoreType(validAst)

    assertEquals(errors, Vector.empty)
  }
}
