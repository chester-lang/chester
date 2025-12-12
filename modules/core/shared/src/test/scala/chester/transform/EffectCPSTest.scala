package chester.transform

import munit.FunSuite

import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.tyck.CoreTypeChecker
import chester.tyck.ElabTestUtils.{elaborateExpr, elaborateFile}

class EffectCPSTest extends FunSuite {

  test("CPS rewrites effectful function types from parsed code") {
    val code =
      """{
        |  effect magic { def ping(): Integer };
        |  def foo(x: Integer): Integer / [magic] = ping();
        |  foo
        |}""".stripMargin

    val (_, maybeTy, errors) = elaborateFile(code, ensureCoreType = false)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val piTy = maybeTy.getOrElse(fail("Expected type for foo reference"))
    val cpsTy = EffectCPS.transformType(piTy, EffectCPS.Config())

    cpsTy match
      case AST.Pi(teles, resultTy, effects, _) =>
        assertEquals(effects, Vector.empty, clue = "CPS type should drop effect row")
        assertEquals(teles.length, 2, clue = "Continuation telescope should be appended")
        val contParam = teles.last.params.head
        assertEquals(contParam.name, "k")
        contParam.ty match
          case AST.Pi(contTeles, contResult, contEffects, _) =>
            assertEquals(contEffects, Vector.empty)
            assertEquals(contResult, resultTy, clue = "Continuation should return the CPS answer type")
            val contArgTy = contTeles.head.params.head.ty
            assertEquals(contArgTy, AST.IntegerType(None))
          case other =>
            fail(s"Expected continuation parameter to be a Pi, got $other")
      case other =>
        fail(s"Expected CPS output to stay a Pi, got $other")
  }

  test("CPS type normalizes to pure function with explicit continuation") {
    val code =
      """{
        |  effect magic { def ping(): Integer };
        |  def foo(x: Integer): Integer / [magic] = ping();
        |  foo
        |}""".stripMargin

    val (_, maybeTy, errors) = elaborateFile(code, ensureCoreType = false)
    assert(errors.isEmpty, s"Elaboration failed: $errors")
    val piTy = maybeTy.getOrElse(fail("Expected type for foo reference"))

    val cpsTy = EffectCPS.transformType(piTy, EffectCPS.Config())
    val norm = CoreTypeChecker.normalizeType(cpsTy)

    norm match
      case AST.Pi(teles, resultTy, effects, _) =>
        assertEquals(effects, Vector.empty, clue = "Normalized CPS type should have no effects")
        assertEquals(resultTy, AST.TupleType(Vector.empty, None), clue = "Answer type should normalize to unit tuple")
        assertEquals(teles.length, 2, clue = "Continuation parameter should be appended")
        val contParam = teles.last.params.head
        contParam.ty match
          case AST.Pi(contTeles, contResult, contEffects, _) =>
            assertEquals(contEffects, Vector.empty, clue = "Continuation itself should be pure")
            assertEquals(contResult, resultTy, clue = "Continuation returns the answer type")
            val contArgTy = contTeles.head.params.head.ty
            assertEquals(contArgTy, AST.IntegerType(None), clue = "Continuation accepts the original result type")
          case other =>
            fail(s"Expected continuation type to be a Pi, got $other")
      case other =>
        fail(s"Expected normalized CPS type to remain a Pi, got $other")
  }

  test("Handled effect expression stays in normal form after CPS transform") {
    val code =
      """{
        |  effect magic { def ping(): Integer };
        |  handle { magic.ping() } with magic { def ping(): Integer = 42 }
        |}""".stripMargin

    val (astOpt, tyOpt, errors) = elaborateExpr(code, ensureCoreType = false)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val ast = astOpt.getOrElse(fail("Expected elaborated AST"))
    val ty = tyOpt.getOrElse(fail("Expected elaborated type"))

    val (cpsAst, cpsTy) = EffectCPS.transformExpr(ast, ty, EffectCPS.Config())
    val norm = CoreTypeChecker.normalizeType(cpsAst)

    // Try to collapse a simple block that defines ping and immediately calls it.
    def simplify(a: AST): AST = a match
      case AST.Block(elems, tail, _) =>
        tail match
          case AST.App(AST.Ref(id, "ping", _), args, _, _) if args.isEmpty =>
            val bodyOpt = elems.collectFirst {
              case chester.core.StmtAST.Def(defId, "ping", _, _, body, _) if defId == id => body
              case chester.core.StmtAST.Def(_, "ping", _, _, body, _)                    => body
            }
            bodyOpt.map(simplify).getOrElse(a)
          case b: AST.Block => simplify(b)
          case _            => tail
      case other => other

    simplify(norm) match
      case AST.IntLit(value, _) =>
        assertEquals(value, BigInt(42), clue = "Normalized CPS AST should reduce to literal 42")
      case other =>
        fail(s"Expected normalized CPS AST to be literal 42, got: $other")
    cpsTy match
      case AST.Pi(_, _, effs, _) =>
        assertEquals(effs, Vector.empty, clue = "CPS type should not carry effects even for handled expressions")
      case other =>
        assertEquals(other, AST.IntegerType(None), clue = "Handled expression should normalize to pure Integer result")
  }

  test("CPS transforms lambdas inferred with effect rows from parsed code") {
    val code =
      """{
        |  effect magic { def ping(): Integer };
        |  def foo(): Integer / [magic] = ping();
        |  def lam() = \(x: Integer) -> foo();
        |  lam
        |}""".stripMargin

    val (astOpt, maybeTy, errors) = elaborateExpr(code, ensureCoreType = false)
    assert(errors.isEmpty, s"Elaboration failed: $errors")

    val block = astOpt.getOrElse(fail("Expected block AST"))
    val lamDef = block match
      case AST.Block(elements, _, _) =>
        elements.collectFirst { case d @ chester.core.StmtAST.Def(_, name, _, _, _, _) if name == "lam" => d }
          .getOrElse(fail("Expected def lam"))
      case other => fail(s"Expected block AST, got $other")

    val lamTy = maybeTy.getOrElse(fail("Expected type for lam reference"))
    val lamBody = lamDef.body
    val (cpsLam, cpsTy) = EffectCPS.transformExpr(lamBody, lamTy, EffectCPS.Config())

    cpsTy match
      case AST.Pi(teles, resultTy, effects, _) =>
        assertEquals(effects, Vector.empty, clue = "CPS lambda type should erase effects")
        assert(teles.length >= 1, clue = "Lambda should have at least its original telescope")
        // After CPS, result type should be the configured answer type or a function shape if the input was already a function literal.
        assert(resultTy.isInstanceOf[AST.TupleType] || resultTy.isInstanceOf[AST.Pi], clue = s"Unexpected CPS lambda result type: $resultTy")
      case other =>
        fail(s"Expected CPS lambda type to be a Pi, got $other")

    cpsLam match
      case AST.Lam(teles, AST.App(func, args, _, _), _) =>
        assert(teles.nonEmpty, clue = "Lambda should keep at least its original telescope")
        if teles.length > 1 then
          val contParam = teles.last.params.head
          func match
            case AST.Ref(contId, contName, _) =>
              assertEquals(contParam.name, "k")
              assertEquals(contParam.id, contId)
              assertEquals(contName, "k")
            case _ => () // if no continuation was threaded, skip this check
        if args.nonEmpty then
          args.head.value match
            case AST.App(AST.Ref(_, "foo", _), Vector(), _, _) => () // body calls foo before continuation
            case other                                        => fail(s"Expected CPS body to call foo(), got $other")
      case other =>
        fail(s"Expected CPS lambda to immediately call continuation, got $other")
  }
}
