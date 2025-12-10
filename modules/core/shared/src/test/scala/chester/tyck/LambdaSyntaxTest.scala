package chester.tyck

import munit.FunSuite

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, StmtAST}
import chester.tyck.ElabTestUtils.elaborateExpr

class LambdaSyntaxTest extends FunSuite {

  test("lambda expression with backslash syntax elaborates to AST.Lam") {
    val (astOpt, _, errors) = elaborateExpr("""\x -> x""", ensureCoreType = true)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    astOpt match
      case Some(AST.Lam(telescopes, body, _)) =>
        val params = telescopes.flatMap(_.params)
        assertEquals(params.length, 1)
        assertEquals(params.head.name, "x")
        assert(body.isInstanceOf[AST.Ref], s"Expected lambda body to be a reference, got $body")
      case other => fail(s"Expected AST.Lam, got $other")
  }

  test("lambda used as type parses to Pi when in a type position") {
    val code =
      """{ def id: \(A: Type) -> A = \x -> x;
        |  id }""".stripMargin
    val (astOpt, _, errors) = elaborateExpr(code, ensureCoreType = true)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val block = astOpt.getOrElse(fail("Expected AST.Block from elaboration"))
    block match
      case AST.Block(elements, _, _) =>
        val defStmt = elements.collectFirst { case d: StmtAST.Def => d }.getOrElse(fail("Expected def id"))
        val pi = defStmt.resultTy.getOrElse(fail("Expected annotated return type"))
        pi match
          case AST.Pi(telescopes, result, effects, _) =>
            assertEquals(telescopes.length, 1)
            assertEquals(telescopes.head.params.head.name, "A")
            assert(effects.isEmpty, s"Unexpected effects on Pi: $effects")
            result match
              case AST.MetaCell(_, _)         => () // placeholder until solved; just ensure shape
              case AST.Ref(_, "A", _)         => () // resolved reference to type parameter
              case other                      => fail(s"Expected meta result type or A, got $other")
          case other => fail(s"Expected Pi type from type-level lambda, got $other")
      case other => fail(s"Expected block AST, got $other")
  }
}
