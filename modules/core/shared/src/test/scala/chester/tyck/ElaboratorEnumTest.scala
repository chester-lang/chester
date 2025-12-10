package chester.tyck

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.tyck.ElabTestUtils.{defaultTimeout, elaborateExpr, runAsync, given ExecutionContext}
import munit.FunSuite

class ElaboratorEnumTest extends FunSuite:

  override val munitTimeout: FiniteDuration = defaultTimeout

  test("enum case reference has enum type") {
    val code =
      """{ enum Color { case Red; case Green };
        |  Color.Red }""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborateExpr(code, ensureCoreType = true)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.EnumTypeRef(_, name, _)) => assertEquals(name, "Color")
        case other                             => fail(s"Expected EnumTypeRef(Color), got $other")
    }
  }

  test("enum case with payload typechecks") {
    val code =
      """{ enum Maybe { case Some(x: Integer); case None };
        |  Maybe.Some(42) }""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborateExpr(code, ensureCoreType = true)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.EnumTypeRef(_, name, _)) => assertEquals(name, "Maybe")
        case other                             => fail(s"Expected EnumTypeRef(Maybe), got $other")
    }
  }

  test("enum type reference via .t has universe type") {
    val code =
      """{ enum Result { case Ok };
        |  Result.t }""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborateExpr(code, ensureCoreType = true)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.Type(AST.LevelLit(level, _), _)) => assertEquals(level, BigInt(0))
        case other                                     => fail(s"Expected Type(LevelLit(0)), got $other")
    }
  }

  test("enum statement accepts trailing semicolon") {
    val code =
      """{ enum Maybe { case Some(x: Integer); case None };
        |  42 }""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborateExpr(code, ensureCoreType = true)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.IntegerType(_)) => () // tail is 42
        case other                    => fail(s"Expected Integer type, got $other")
    }
  }

  test("enum statement without semicolon is rejected") {
    val code =
      """{ enum Maybe { case Some(x: Integer); case None }
        |  42 }""".stripMargin

    runAsync {
      val (_, _, errors) = elaborateExpr(code)
      assert(errors.exists(_.toString.contains("enum statement")), s"Expected enum placement error, got $errors")
    }
  }

  test("coenum case reference has enum type") {
    val code =
      """{ coenum Ping { case Pong };
        |  Ping.Pong }""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborateExpr(code, ensureCoreType = true)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.EnumTypeRef(_, name, _)) => assertEquals(name, "Ping")
        case other                             => fail(s"Expected EnumTypeRef(Ping), got $other")
    }
  }

  test("parameterized enum supports type arguments") {
    val code =
      """{ enum MyList(a: Type) {
        |    case Nil;
        |    case Cons(head: a, tail: MyList.t(a))
        |  };
        |  MyList.Cons(String, "hi", MyList.Nil(String))
        |}""".stripMargin

    runAsync {
      val (_, tyOpt, errors) = elaborateExpr(code, ensureCoreType = true)

      assert(errors.isEmpty, s"Should not have errors, got $errors")
      tyOpt match
        case Some(AST.App(AST.EnumTypeRef(_, name, _), args, _, _)) =>
          assertEquals(name, "MyList")
          assertEquals(args.length, 1)
          args.head.value match
            case AST.StringType(_) => ()
            case other             => fail(s"Expected String type argument, got $other")
        case other => fail(s"Expected applied MyList type, got $other")
    }
  }
