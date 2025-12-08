package chester.syntax

import munit.FunSuite
import chester.reader.{FileNameAndContent, Source}

import scala.language.experimental.genericNumberLiterals

class TypeScriptDeclParserTest extends FunSuite {
  test("parse simple interface") {
    val source = """
      interface Person {
        name: string;
        age: number;
      }
    """
    val sourceRef = Source(FileNameAndContent("test.d.ts", source))
    val ast = TypeScriptDeclParser.parse(source, sourceRef)

    ast match {
      case TypeScriptAST.Program(statements, _) =>
        assertEquals(statements.length, 1, "Should have one statement")
        statements.head match {
          case TypeScriptAST.InterfaceDeclaration(name, typeParams, extendsTypes, members, _) =>
            assertEquals(name, "Person", "Interface name should be Person")
            assertEquals(members.length, 2, "Should have two members")
          case other =>
            fail(s"Expected InterfaceDeclaration, got $other")
        }
      case other =>
        fail(s"Expected Program, got $other")
    }
  }

  test("parse type alias") {
    val source = """
      type ID = string;
    """
    val sourceRef = Source(FileNameAndContent("test.d.ts", source))
    val ast = TypeScriptDeclParser.parse(source, sourceRef)

    ast match {
      case TypeScriptAST.Program(statements, _) =>
        assertEquals(statements.length, 1, "Should have one statement")
        statements.head match {
          case TypeScriptAST.TypeAliasDeclaration(name, typeParams, aliasType, _) =>
            assertEquals(name, "ID", "Type alias name should be ID")
            aliasType match {
              case TypeScriptType.PrimitiveType(typeName, _) =>
                assertEquals(typeName, "string", "Alias type should be string")
              case other =>
                fail(s"Expected PrimitiveType, got $other")
            }
          case other =>
            fail(s"Expected TypeAliasDeclaration, got $other")
        }
      case other =>
        fail(s"Expected Program, got $other")
    }
  }

  test("parse empty file") {
    val source = ""
    val sourceRef = Source(FileNameAndContent("test.d.ts", source))
    val ast = TypeScriptDeclParser.parse(source, sourceRef)

    ast match {
      case TypeScriptAST.Program(statements, _) =>
        assertEquals(statements.length, 0, "Should have no statements")
      case other =>
        fail(s"Expected Program, got $other")
    }
  }
}
