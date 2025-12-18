package chester.backend

import munit.FunSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.syntax.{InterfaceMemberType, TypeScriptAST, TypeScriptType}
import chester.tyck.ElabTestUtils
import chester.tyck.ElabTestUtils.{defaultTimeout, runAsync, given ExecutionContext}
import chester.utils.doc.{DocConf, render, *, given}

class TypeScriptBackendTest extends FunSuite:

  private given DocConf = DocConf.Default

  override val munitTimeout: FiniteDuration = defaultTimeout

  private def normalize(output: String): String =
    output.linesIterator.map(_.trim).filter(_.nonEmpty).mkString("\n").trim

  test("function def lowers to function declaration and tail return") {
    runAsync {
      val code =
        """{ def id(x: Integer) = x;
          |  id }""".stripMargin
      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")
      val program = TypeScriptBackend.lowerProgram(astOpt.get)

      val fnDecl = program.statements.collectFirst { case fn: TypeScriptAST.FunctionDeclaration => fn }
      assert(fnDecl.isDefined, "Expected a function declaration")
      assertEquals(fnDecl.get.name, "id")

      program.statements.lastOption match
        case Some(TypeScriptAST.Return(Some(TypeScriptAST.Identifier("id", _)), _)) => ()
        case other                                                                 => fail(s"Expected return of id, got $other")
    }
  }

  test("backend renders a simple program shape") {
    runAsync {
      val code =
        """{ record Box(value: String);
          |  def add1(x: Integer) = x + 1;
          |  add1(41) }""".stripMargin

      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")

      val program = TypeScriptBackend.lowerProgram(astOpt.get)
      val rendered = normalize(render(program.toDoc).toString)

      val expected = normalize(
        """interface Box {
          |value: string;
          |};
          |function add1(x: number) {
          |return x + 1;
          |};
          |return add1(41);""".stripMargin
      )

      assertEquals(rendered, expected)
    }
  }

  test("record lowers to interface declaration") {
    runAsync {
      val code =
        """{ record Box(value: String);
          |  () }""".stripMargin
      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")
      val program = TypeScriptBackend.lowerProgram(astOpt.get)

      program.statements.collectFirst { case iface: TypeScriptAST.InterfaceDeclaration => iface } match
        case Some(TypeScriptAST.InterfaceDeclaration(name, _, _, members, _)) =>
          assertEquals(name, "Box")
          assert(members.exists(_.name == "value"), "Field 'value' should be present")
          members.head.memberType match
            case InterfaceMemberType.PropertySignature(tpe, _, _) =>
              tpe match
                case TypeScriptType.PrimitiveType("string", _) => ()
                case other                                    => fail(s"Expected string property, got $other")
            case other => fail(s"Expected property signature, got $other")
        case None => fail("Expected interface declaration in lowered program")
    }
  }

  test("JSImport lowers to TypeScript import declaration") {
    runAsync {
      val code =
        """{ import * as fs from "node:fs";
          |  () }""".stripMargin
      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")
      val program = TypeScriptBackend.lowerProgram(astOpt.get)

      val rendered = normalize(render(program.toDoc).toString)
      assert(rendered.contains("""import * as fs from "node:fs";"""), s"Expected import declaration, got:\n$rendered")
    }
  }
