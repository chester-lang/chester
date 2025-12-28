package chester.syntax

import scala.language.experimental.genericNumberLiterals

import munit.FunSuite
import chester.reader.{FileNameAndContent, Source}

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

  test("parse export interface and keep spans") {
    val source = """
      export interface Box {
        value: number;
      }
    """
    val sourceRef = Source(FileNameAndContent("box.d.ts", source))
    val ast = TypeScriptDeclParser.parse(source, sourceRef)

    ast match {
      case TypeScriptAST.Program(statements, _) =>
        statements.headOption match {
          case Some(TypeScriptAST.ExportDeclaration(Some(inner), _, _, isDefault, exportSpan)) =>
            assert(!isDefault, "export should not be default in this sample")
            assert(exportSpan.exists(s => s.range.start.index < s.range.end.index), "export span should cover content")
            inner match {
              case TypeScriptAST.InterfaceDeclaration(_, _, _, members, ifaceSpan) =>
                assert(ifaceSpan.exists(s => s.range.start.index < s.range.end.index), "interface span should cover content")
                assertEquals(members.length, 1, "Should have one exported member")
                val memberSpan = members.head.span.getOrElse(fail("member span missing"))
                assert(memberSpan.range.start.index < memberSpan.range.end.index, "member span should advance")
              case other =>
                fail(s"Expected InterfaceDeclaration inside export, got $other")
            }
          case other =>
            fail(s"Expected ExportDeclaration with inner interface, got $other")
        }
      case other =>
        fail(s"Expected Program, got $other")
    }
  }

  test("parse declare function signature") {
    val source = "declare function greet(name: string): void;"
    val sourceRef = Source(FileNameAndContent("decls.d.ts", source))
    val ast = TypeScriptDeclParser.parse(source, sourceRef)

    ast match {
      case TypeScriptAST.Program(Vector(func: TypeScriptAST.FunctionDeclaration), _) =>
        assertEquals(func.name, "greet")
        assert(func.modifiers.contains(Modifier.Declare), "declare keyword should become a modifier")
        assertEquals(func.params.length, 1, "Function should have one parameter")
        func.params.head.paramType match {
          case Some(TypeScriptType.PrimitiveType("string", _)) => ()
          case other                                          => fail(s"Expected string parameter type, got $other")
        }
        func.returnType match {
          case Some(TypeScriptType.PrimitiveType("void", _)) => ()
          case other                                         => fail(s"Expected void return type, got $other")
        }
      case other =>
        fail(s"Expected single function declaration, got $other")
    }
  }

  test("parse optional parameters without hanging") {
    val source = "declare function f(x?: string, y?: number): void;"
    val sourceRef = Source(FileNameAndContent("optional-params.d.ts", source))
    val ast = TypeScriptDeclParser.parse(source, sourceRef)

    ast match {
      case TypeScriptAST.Program(Vector(func: TypeScriptAST.FunctionDeclaration), _) =>
        assertEquals(func.name, "f")
        assert(func.params.length >= 2, s"Expected at least two parameters, got ${func.params.length}")
      case other =>
        fail(s"Expected single function declaration, got $other")
    }
  }

  test("parse interface method signatures") {
    val source =
      """|interface Console {
         |  log(message?: string, ...optionalParams: any[]): void;
         |}
         |""".stripMargin
    val sourceRef = Source(FileNameAndContent("console.d.ts", source))
    val ast = TypeScriptDeclParser.parse(source, sourceRef)

    ast match {
      case TypeScriptAST.Program(Vector(TypeScriptAST.InterfaceDeclaration(_, _, _, members, _)), _) =>
        members.headOption match
          case Some(InterfaceMember("log", InterfaceMemberType.MethodSignature(params, returnType), _)) =>
            assert(params.nonEmpty, "Expected method parameters to be parsed")
            returnType match
              case TypeScriptType.PrimitiveType("void", _) => ()
              case other                                   => fail(s"Expected void return type, got $other")
          case other =>
            fail(s"Expected method signature member, got $other")
      case other =>
        fail(s"Expected Program with interface, got $other")
    }
  }

  test("parse export assignment") {
    val source =
      """|declare module "node:console" {
         |  export = globalThis.console;
         |}
         |""".stripMargin
    val sourceRef = Source(FileNameAndContent("export-assign.d.ts", source))
    val ast = TypeScriptDeclParser.parse(source, sourceRef)

    ast match {
      case TypeScriptAST.Program(Vector(TypeScriptAST.NamespaceDeclaration(_, body, _)), _) =>
        body.collectFirst { case TypeScriptAST.ExportAssignment(_, _) => () }.getOrElse(fail("Missing export assignment"))
      case other =>
        fail(s"Expected module namespace with export assignment, got $other")
    }
  }

  test("parse snippet from NodeJS process.d.ts") {
    val source =
      """|/**
         | * Extracted from @types/node@22.7.0 (process.d.ts)
         | * Memory and CPU usage shapes are simple enough to exercise the declaration parser.
         | */
         |interface MemoryUsage {
         |  rss: number;
         |  heapTotal: number;
         |  heapUsed: number;
         |  external: number;
         |  arrayBuffers: number;
         |}
         |
         |interface CpuUsage {
         |  user: number;
         |  system: number;
         |}
         |
         |interface ProcessRelease {
         |  name: string;
         |  sourceUrl?: string | undefined;
         |  headersUrl?: string | undefined;
         |  libUrl?: string | undefined;
         |  lts?: string | undefined;
         |}
         |""".stripMargin
    val sourceRef = Source(FileNameAndContent("node-process.d.ts", source))
    val ast = TypeScriptDeclParser.parse(source, sourceRef)

    ast match {
      case TypeScriptAST.Program(statements, _) =>
        assertEquals(statements.length, 3, "Should parse three interfaces from NodeJS snippet")
        val names = statements.collect { case TypeScriptAST.InterfaceDeclaration(name, _, _, _, _) => name }.toSet
        assertEquals(names, Set("MemoryUsage", "CpuUsage", "ProcessRelease"))

        val processReleaseMembers = statements.collectFirst {
          case TypeScriptAST.InterfaceDeclaration("ProcessRelease", _, _, members, _) => members
        }.getOrElse(fail("ProcessRelease interface should be present"))

        val sourceUrlMember = processReleaseMembers.find(_.name == "sourceUrl").getOrElse(fail("sourceUrl member missing"))
        sourceUrlMember.memberType match {
          case InterfaceMemberType.PropertySignature(TypeScriptType.UnionType(types, _), isOptional, _) =>
            assert(isOptional, "sourceUrl should be optional")
            val typeNames = types.map {
              case TypeScriptType.PrimitiveType(name, _)  => name
              case TypeScriptType.TypeReference(name, _, _) => name
              case other                                   => fail(s"Unexpected type in union: $other")
            }.toSet
            assertEquals(typeNames, Set("string", "undefined"))
          case other =>
            fail(s"Expected union property signature for sourceUrl, got $other")
        }
      case other =>
        fail(s"Expected Program with interfaces, got $other")
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
