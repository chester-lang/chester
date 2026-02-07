package chester.tyck

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.tyck.ElabTestUtils.{defaultTimeout, elaborateExpr, elaborateModule, runAsync, given ExecutionContext}
import munit.FunSuite

class SubtypingTest extends FunSuite:

  override val munitTimeout: FiniteDuration = defaultTimeout

  test("Any type has type Type(0)") {
    runAsync {
      val (ast, ty, errors) = elaborateExpr("Any")

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ast.isDefined, "AST should be defined")

      ast.get match {
        case AST.AnyType(_) => // OK
        case other          => fail(s"Expected AnyType, got: $other")
      }

      ty.get match {
        case AST.Type(AST.LevelLit(level, _), _) =>
          assertEquals(level, BigInt(0), "Any should have type Type(0)")
        case other => fail(s"Expected Type(0), got: $other")
      }
    }
  }

  test("Any type can be used in type position") {
    runAsync {
      val (ast, ty, errors) = elaborateExpr("{ def f(x: Any) = x; 42 }")

      // Should not have errors about def placement or type errors
      assert(!errors.exists(_.toString.contains("def statement only allowed")), s"Should not have def placement error, got: $errors")
      assert(!errors.exists(_.toString.contains("Type mismatch")), s"Should not have type errors with Any, got: $errors")
      assert(ast.isDefined, "AST should be defined")

      // Just verify it's a block - the Any type should work without errors
      ast.get match {
        case AST.Block(_, _, _) => // OK
        case other              => fail(s"Expected Block, got: $other")
      }
    }
  }

  test("Any can accept any value") {
    runAsync {
      val (ast, ty, errors) = elaborateExpr("{ def f(x: Any) = x; f(42) }")

      // Should elaborate without type errors
      assert(!errors.exists(_.toString.contains("Type mismatch")), s"Should not have type mismatch, got: $errors")
    }
  }

  test("type check id[id(String)](\"a\") reduces implicit type argument") {
    runAsync {
      val (ast, ty, errors) = elaborateExpr("""{
        def id[a: Type(0)](x: a) = x;
        id[id(String)]("a")
      }""")

      assert(errors.isEmpty, s"Should type check with reduction, got errors: $errors")
      assert(ast.isDefined, "AST should be defined")
      assert(ty.isDefined, "Type should be defined")

      ty.get match {
        case AST.StringType(_) => // expected
        case other             => fail(s"Expected String type, got: $other")
      }
    }
  }

  test("annotated integer list has type List(Integer)") {
    runAsync {
      val (ast, ty, errors) = elaborateExpr("[1]: List(Integer)", ensureCoreType = true)

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ast.isDefined, "AST should be defined")
      assert(ty.isDefined, "Type should be defined")

      Elaborator.normalizeType(ty.get) match
        case AST.ListType(elem, _) => assert(elem.isInstanceOf[AST.IntegerType], s"Expected Integer element type, got: $elem")
        case other                 => fail(s"Expected List type, got: $other")
    }
  }

  test("annotated string list has type List(String)") {
    runAsync {
      val (ast, ty, errors) = elaborateExpr("""["a"]: List(String)""", ensureCoreType = true)

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ast.isDefined, "AST should be defined")
      assert(ty.isDefined, "Type should be defined")

      Elaborator.normalizeType(ty.get) match
        case AST.ListType(elem, _) => assert(elem.isInstanceOf[AST.StringType], s"Expected String element type, got: $elem")
        case other                 => fail(s"Expected List type, got: $other")
    }
  }

  test("integer literal defaults to Integer") {
    runAsync {
      val (_, ty, errors) = elaborateExpr("42", ensureCoreType = true)
      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ty.isDefined, "Type should be defined")
      ty.get match {
        case AST.IntegerType(_) => ()
        case other              => fail(s"Expected Integer type, got: $other")
      }
    }
  }

  test("annotated Natural literal is Natural") {
    runAsync {
      val (_, ty, errors) = elaborateExpr("42: Natural", ensureCoreType = true)
      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ty.isDefined, "Type should be defined")
      ty.get match {
        case AST.NaturalType(_) => ()
        case other              => fail(s"Expected Natural type, got: $other")
      }
    }
  }

  test("type check id(42) first") {
    runAsync {
      // Simpler test: just id(42)
      val (ast, ty, errors) = elaborateExpr("""{
        def id[a: Type(0)](x: a) = x;
        id(42)
      }""")

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ty.isDefined, "Type should be defined")
    }
  }

  test("type check id(id) returns id") {
    runAsync {
      // Simpler test: just id(id) without the second application
      val (ast, ty, errors) = elaborateExpr("""{
        def id[a: Type(0)](x: a) = x;
        id(id)
      }""")

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ty.isDefined, "Type should be defined")

      // The type should be [a: Type(0)](x: a) -> a
      ty.get match {
        case AST.Pi(_, _, _, _) => // OK - result type is a function type
        case other              => fail(s"Expected Pi type, got: $other")
      }
    }
  }

  test("type check id(id)(\"a\") with type String") {
    runAsync {
      // id : [a: Type(0)](x: a) -> a
      // id(id) applies id to itself: id[([a:Type](x:a)->a)](id) : ([a:Type](x:a)->a)
      // So id(id) returns id, and id(id)("a") should be the same as id("a")
      // The type of "a") is String, so result should have type String
      val (ast, ty, errors) = elaborateExpr("""{
        def id[a: Type(0)](x: a) = x;
        id(id)("a")
      }""")

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ast.isDefined, "AST should be defined")
      assert(ty.isDefined, "Type should be defined")

      // The type should be String (the type of "a")
      ty.get match {
        case AST.StringType(_) => // OK - result type is String
        case other             => fail(s"Expected String type, got: $other")
      }
    }
  }

  test("type check annotated id(id)(\"a\"): String") {
    runAsync {
      val (ast, ty, errors) = elaborateExpr("""{
        def id[a: Type(0)](x: a) = x;
        id(id)("a"): String
      }""")

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ast.isDefined, "AST should be defined")
      assert(ty.isDefined, "Type should be defined")

      ty.get match {
        case AST.StringType(_) => // expected
        case other             => fail(s"Expected String type, got: $other")
      }
    }
  }

  test("builtin println carries io effect and can be used in annotated function") {
    runAsync {
      val (_, printlnTy, printlnErrors) = elaborateExpr("println")
      assert(printlnErrors.isEmpty, s"println lookup should be error free, got: $printlnErrors")

      printlnTy match
        case Some(AST.Pi(_, _, effects, _)) =>
          assert(effects.map(_.name).contains("io"), s"Expected io effect in println type, got: $effects")
        case other => fail(s"Expected println to have function type, got: $other")

      val (_, _, defErrors) = elaborateExpr("""{
        def log(msg: String): Unit / [io] = println(msg);
        log("hi")
      }""")
      assert(defErrors.isEmpty, s"Function using println should elaborate without errors, got: $defErrors")
    }
  }

  test("println cannot be used where no effects are allowed") {
    runAsync {
      val (_, _, errors) = elaborateExpr("""{
        def bad(msg: String): Unit / [] = println(msg);
        bad("oops")
      }""")

      assert(errors.nonEmpty, "Expected error when using println in effect-free function")
    }
  }

  test("cross-file defs can reference each other") {
    runAsync {
      val (_, tys, errors) = elaborateModule(
        Seq(
          "package p; def f(x: String): String = g(x);",
          "package p; def g(y: String): String = y; def main(): Unit / [io] = println(f(\"hi\"));"
        )
      )

      assert(errors.isEmpty, s"Expected no errors, got: $errors")
    }
  }

  test("let binding is sequential") {
    runAsync {
      val (_, ty, errors) = elaborateExpr("""{
        let x = 42;
        x
      }""")

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ty.isDefined, "Type should be defined")

      ty.get match
        case AST.IntegerType(_) => ()
        case other              => fail(s"Expected Integer type, got: $other")
    }
  }

  test("let binding supports shadowing") {
    runAsync {
      val (_, ty, errors) = elaborateExpr("""{
        let x = 42;
        let x = "shadow";
        x
      }""")

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ty.isDefined, "Type should be defined")
      ty.get match
        case AST.StringType(_) => ()
        case other             => fail(s"Expected String type, got: $other")
    }
  }

  test("let binding respects annotations") {
    runAsync {
      val (_, ty, errors) = elaborateExpr("""{
        let greeting: String = "hello";
        greeting
      }""")

      assert(errors.isEmpty, s"Should have no errors, got: $errors")
      assert(ty.isDefined, "Type should be defined")
      ty.get match
        case AST.StringType(_) => ()
        case other             => fail(s"Expected String type, got: $other")
    }
  }

  test("let bindings cannot reference future declarations") {
    runAsync {
      val (_, _, errors) = elaborateExpr("""{
        let y = x;
        let x = 42;
        y
      }""")

      assert(errors.exists(_.toString.contains("Unbound variable")), s"Should report unbound variable, got: $errors")
    }
  }

  test("record constructor and field access typecheck") {
    runAsync {
      val (_, ty, errors) = elaborateExpr("""{
        record Vec2d(x: Integer, y: Integer);
        let v: Vec2d.t = Vec2d(1, 2);
        v.x
      }""")

      assert(errors.isEmpty, s"Expected no errors, got: $errors")
      assert(ty.isDefined, "Type should be defined")
      ty.get match
        case AST.IntegerType(_) => ()
        case other              => fail(s"Expected Integer field type, got: $other")
    }
  }
