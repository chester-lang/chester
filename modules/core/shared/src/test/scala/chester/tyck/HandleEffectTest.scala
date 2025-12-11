package chester.tyck

import munit.FunSuite

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, StmtAST}
import chester.tyck.ElabTestUtils.elaborateFile

/** End-to-end test for the desugared handle/with effect pattern.
  * The handler provides an implementation for `magic.ping` that returns 42.
  * The handled block invokes `magic.ping()` and should elaborate and evaluate to 42.
  */
class HandleEffectTest extends FunSuite {

  test("handle with user effect provides operation implementation") {
    val code =
      """{
        |  effect magic { def ping(): Integer };
        |  handle {
        |    magic.ping()
        |  } with magic {
        |    def ping(): Integer = 42
        |  }
        |}""".stripMargin

    val (astOpt, tyOpt, errors) = elaborateFile(code, ensureCoreType = true)
    assert(errors.isEmpty, s"Elaboration errors: $errors")

    val ast = astOpt.getOrElse(fail("Expected elaborated AST"))
    val ty = tyOpt.getOrElse(fail("Expected elaborated type"))

    // The elaborated AST should contain the handler-provided def `ping` and a call to it.
    def containsDefPing(ast: AST): Boolean = ast match
      case AST.Block(elements, tail, _) =>
        elements.exists {
          case StmtAST.Def(_, name, _, _, _, _) => name == "ping"
          case _                                => false
        } || containsDefPing(tail)
      case _ => false

    def containsCallPing(ast: AST): Boolean = ast match
      case AST.App(AST.Ref(_, "ping", _), args, _, _) => args.isEmpty
      case AST.Block(elements, tail, _) =>
        elements.exists {
          case StmtAST.ExprStmt(expr, _) => containsCallPing(expr)
          case StmtAST.Def(_, _, _, _, body, _) =>
            containsCallPing(body)
          case _ => false
        } || containsCallPing(tail)
      case AST.Let(_, _, _, value, body, _) =>
        containsCallPing(value) || containsCallPing(body)
      case AST.Tuple(elems, _)   => elems.exists(containsCallPing)
      case AST.ListLit(elems, _) => elems.exists(containsCallPing)
      case AST.Ann(expr, ty, _)  => containsCallPing(expr) || containsCallPing(ty)
      case _                     => false

    assert(containsDefPing(ast), s"Expected handler to define ping somewhere in AST: $ast")
    assert(containsCallPing(ast), s"Expected ping() call in AST: $ast")

    // Type should be Integer with no residual effects.
    ty match
      case AST.IntegerType(_) => ()
      case other              => fail(s"Expected Integer type, got: $other")
  }
}
