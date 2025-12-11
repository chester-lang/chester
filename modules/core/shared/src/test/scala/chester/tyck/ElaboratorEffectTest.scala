package chester.tyck

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.tyck.ElabTestUtils.{defaultTimeout, elaborateFile, runAsync, given ExecutionContext}
import munit.FunSuite

class ElaboratorEffectTest extends FunSuite:

  override val munitTimeout: FiniteDuration = defaultTimeout

  test("propagates user defined effect rows through calls") {
    runAsync {
      val code =
        """{
          |  effect magic;
          |  def foo(): Integer / [magic] = 1;
          |  def bar(): Integer = foo();
          |  bar
          |}""".stripMargin

      val (_, barTy, errors) = elaborateFile(code, ensureCoreType = true)
      assert(errors.isEmpty, s"Expected no errors, got: $errors")

      barTy match
        case Some(AST.Pi(_, _, effects, _)) =>
          assertEquals(effects.map(_.name).toSet, Set("magic"), clue = s"bar should propagate foo's effect, got $effects")
        case other =>
          fail(s"Expected Pi type for bar reference, got: $other")
    }
  }

  test("annotated function must include required effects") {
    runAsync {
      val code =
        """{
          |  effect magic;
          |  def foo(): Integer / [magic] = 1;
          |  def bad(): Integer / [] = foo();
          |  bad
          |}""".stripMargin

      val (_, _, errors) = elaborateFile(code)
      assert(errors.nonEmpty, "Expected error when annotated effects do not cover required effects")
    }
  }

  test("annotation that lists required effect type-checks") {
    runAsync {
      val code =
        """{
          |  effect magic;
          |  def foo(): Integer / [magic] = 1;
          |  def ok(): Integer / [magic] = foo();
          |  ok
          |}""".stripMargin

      val (_, okTy, errors) = elaborateFile(code, ensureCoreType = true)
      assert(errors.isEmpty, s"Expected no errors, got: $errors")

      okTy match
        case Some(AST.Pi(_, _, effects, _)) =>
          assertEquals(effects.map(_.name).toSet, Set("magic"), clue = s"ok should retain declared effect row, got $effects")
        case other =>
          fail(s"Expected Pi type for ok reference, got: $other")
    }
  }

  test("using undeclared effect reports an error") {
    runAsync {
      val code =
        """{
          |  def sneaky(): Integer / [ghost] = 1;
          |  sneaky
          |}""".stripMargin

      val (_, _, errors) = elaborateFile(code)
      assert(errors.nonEmpty, "Expected error when using undeclared effect ghost")
    }
  }

  test("effect block can declare operations") {
    runAsync {
      val code =
        """{
          |  effect magic { def flip(): Integer };
          |  flip
          |}""".stripMargin

      val (_, flipTy, errors) = elaborateFile(code)
      assert(errors.isEmpty, s"Expected no errors, got: $errors")

      flipTy match
        case Some(AST.Pi(_, resultTy, effects, _)) =>
          assert(effects.map(_.name).contains("magic"), clue = s"flip should carry magic effect, got $effects")
          resultTy match
            case _: AST.IntegerType => ()
            case meta if meta.isInstanceOf[AST.MetaCell] =>
              // allow meta if still unresolved; it should eventually resolve to Integer
              ()
            case other => fail(s"flip should return Integer, got $other")
        case other => fail(s"Expected function type for flip, got: $other")
    }
  }

  test("operation effects must be handled when called") {
    runAsync {
      val code =
        """{
          |  effect magic { def flip(): Integer };
          |  def ok(): Integer / [magic] = flip();
          |  def bad(): Integer / [] = flip();
          |  ok
          |}""".stripMargin

      val (_, _, errors) = elaborateFile(code)
      assert(errors.nonEmpty, "Expected error when calling flip without required effect annotation in bad")
    }
  }

  test("effects remain lexically scoped within blocks") {
    runAsync {
      val code =
        """{
          |  {
          |    effect local;
          |  };
          |  def bad(): Integer / [local] = 1;
          |  bad
          |}""".stripMargin

      val (_, _, errors) = elaborateFile(code)
      assert(errors.nonEmpty, "Effect declared in inner block should not be visible outside its scope")
    }
  }

  test("outer effects are visible in nested blocks") {
    runAsync {
      val code =
        """{
          |  effect outer;
          |  def outerUse(): Integer / [outer] = 1;
          |  {
          |    def inner(): Integer / [outer] = outerUse();
          |    inner
          |  }
          |}""".stripMargin

      val (_, innerTy, errors) = elaborateFile(code, ensureCoreType = true)
      assert(errors.isEmpty, s"Expected no errors, got: $errors")

      innerTy match
        case Some(AST.Pi(_, _, effects, _)) =>
          assertEquals(effects.map(_.name).toSet, Set("outer"), clue = s"inner should carry the outer effect, got $effects")
        case other =>
          fail(s"Expected Pi type for inner reference, got: $other")
    }
  }
