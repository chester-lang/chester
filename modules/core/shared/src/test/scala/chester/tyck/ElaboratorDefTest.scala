package chester.tyck

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.core.StmtAST
import chester.tyck.ElabTestUtils.{defaultTimeout, elaborateExpr, runAsync, given ExecutionContext}

class ElaboratorDefTest extends munit.FunSuite:

  override val munitTimeout: FiniteDuration = defaultTimeout

  test("elaborate def statement - with implicit parameters - should error at top level") {
    runAsync {
      val (_, _, errors) = elaborateExpr("def id[a: Type](x: a) = x")

      // Since def is in expression position (not in a block), it should produce an error
      assert(errors.nonEmpty, s"Should have errors for def at top level, got: $errors")
      assert(
        errors.exists(_.toString.contains("def statement only allowed in block elements")),
        s"Should have error about def only allowed in block elements, got: $errors"
      )
    }
  }

  test("user-defined id function type") {
    runAsync {
      val (ast, _, errors) = elaborateExpr("{ def id[a: Type](x: a) = x; id }")

      assert(ast.isDefined, "AST should be defined")
      assert(errors.isEmpty, s"Should have no errors, got: $errors")

      ast.get match {
        case AST.Block(elements, tail, _) =>
          assert(elements.nonEmpty, "Block should have def element")
          tail match {
            case AST.Ref(_, "id", _) => // OK - reference to id
            case other               => fail(s"Expected Ref to id in tail, got: $other")
          }
        case other => fail(s"Expected Block, got: $other")
      }
    }
  }

  test("elaborate id function application - inferred type argument") {
    runAsync {
      val (ast, _, errors) = elaborateExpr("{ def id[a: Type](x: a) = x; id(42) }")

      assert(ast.isDefined, s"AST should be defined, errors: $errors")
      assert(!errors.exists(_.toString.contains("Unbound")), s"Should not have unbound errors: $errors")

      // Just verify it's a Block with an application
      ast.get match {
        case AST.Block(elements, _, _) =>
          assert(elements.nonEmpty, "Block should have def element")
        case other =>
          fail(s"Expected Block, got: $other")
      }
    }
  }

  test("elaborate id function application - explicit type argument") {
    runAsync {
      val (ast, _, errors) = elaborateExpr("{ def id[a: Type](x: a) = x; id[Type](42) }")

      assert(ast.isDefined, s"AST should be defined, errors: $errors")
      assert(!errors.exists(_.toString.contains("Unbound")), s"Should not have unbound errors: $errors")

      // Just verify it's a Block with def
      ast.get match {
        case AST.Block(elements, _, _) =>
          assert(elements.nonEmpty, "Block should have def element")
        case other =>
          fail(s"Expected Block, got: $other")
      }
    }
  }

  test("def in tail position is rejected") {
    runAsync {
      val (_, _, errors) = elaborateExpr("{ def f(x: Type) = x }")

      // Should have an error about def in tail position
      assert(errors.nonEmpty, "Should have errors")
      assert(errors.exists(_.toString.contains("tail position")), s"Should have error about tail position, got: $errors")
    }
  }

  test("def in block elements simple") {
    runAsync {
      val (ast, _, errors) = elaborateExpr("{ def f(x: Type) = x; 42 }")

      // Should elaborate without errors about def being disallowed
      assert(
        !errors.exists(_.toString.contains("def statement only allowed in block elements")),
        s"Should not have error about def placement, got: $errors"
      )

      // The AST should be a Block containing a Def in elements and 42 in tail
      ast match {
        case Some(AST.Block(elements, tail, _)) =>
          assert(elements.length >= 1, s"Block should have at least 1 element (def), got: ${elements.length}")
          assert(elements.head.isInstanceOf[StmtAST.Def], s"First element should be Def, got: ${elements.head}")
          assert(tail.isInstanceOf[AST.IntLit], s"Tail should be IntLit, got: $tail")
        case other =>
          fail(s"Expected Block, got: $other")
      }
    }
  }

  test("def in block can be called") {
    runAsync {
      // Test that def in block can be referenced by later expressions
      val (_, _, errors) = elaborateExpr("{ def id[a: Type](x: a) = x; id(42) }")

      // Should not have unbound variable error for id
      assert(!errors.exists(_.toString.contains("Unbound variable")), s"Should not have unbound variable error for id, got: $errors")
    }
  }

  test("record constructor application in block elaborates") {
    runAsync {
      val (_, ty, errors) = elaborateExpr("{ record Pair(x: Integer, y: Integer); Pair(1, 2) }")

      assert(errors.isEmpty, s"Expected no errors, got: $errors")
      assert(ty.nonEmpty, "Type should be produced for record constructor application")
    }
  }

  test("record field access after constructor works in block") {
    runAsync {
      val (_, ty, errors) = elaborateExpr("{ record Pair(x: Integer, y: Integer); let p: Pair.t = Pair(1, 2); p.x }")

      assert(errors.isEmpty, s"Expected no errors, got: $errors")
      ty match
        case Some(AST.IntegerType(_)) => ()
        case other                    => fail(s"Expected Integer type, got: $other")
    }
  }
