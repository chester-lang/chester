package chester.interop.golang

import scala.language.experimental.genericNumberLiterals

import munit.FunSuite
import chester.core.{AST, Implicitness, Param}
import chester.tyck.GoImportSignature

class GoToChesterTest extends FunSuite {

  test("parse simple Go function from JSON") {
    val json = """{
      "package": "os",
      "interfaces": [],
      "structs": [],
      "functions": [
        {
          "name": "Hostname",
          "params": [],
          "results": [
            {"type": "string"},
            {"type": "error"}
          ]
        }
      ]
    }"""

    val sig = GoToChester.packageSignature(json, "os")
    assertEquals(sig.packageName, "os")
    assertEquals(sig.fields.length, 1)

    val hostnameField = sig.fields.head
    assertEquals(hostnameField.name, "Hostname")

    // Should be a function type (Pi) that returns tuple of (string, error)
    hostnameField.ty match
      case AST.Pi(telescopes, returnType, _, _) =>
        assertEquals(telescopes.head.params.length, 0) // No parameters
        returnType match
          case AST.TupleType(types, _) =>
            assertEquals(types.length, 2)
            assert(types(0).isInstanceOf[AST.StringType])
          case other => fail(s"Expected TupleType for multiple return values, got $other")
      case other => fail(s"Expected Pi type for function, got $other")
  }

  test("parse Go struct from JSON") {
    val json = """{
      "package": "os",
      "interfaces": [],
      "structs": [
        {
          "name": "FileInfo",
          "fields": [
            {"name": "Name", "type": "string"},
            {"name": "Size", "type": "int64"}
          ]
        }
      ],
      "functions": []
    }"""

    val sig = GoToChester.packageSignature(json, "os")
    assertEquals(sig.fields.length, 1)

    val fileInfoField = sig.fields.head
    assertEquals(fileInfoField.name, "FileInfo")
    // Structs are mapped to AnyType (type constructor)
    assert(fileInfoField.ty.isInstanceOf[AST.AnyType])
  }

  test("parse Go interface from JSON") {
    val json = """{
      "package": "io",
      "interfaces": [
        {
          "name": "Reader",
          "methods": [
            {
              "name": "Read",
              "params": [
                {"name": "p", "type": "[]byte"}
              ],
              "results": [
                {"type": "int"},
                {"type": "error"}
              ]
            }
          ]
        }
      ],
      "structs": [],
      "functions": []
    }"""

    val sig = GoToChester.packageSignature(json, "io")
    assertEquals(sig.fields.length, 1)

    val readerField = sig.fields.head
    assertEquals(readerField.name, "Reader")
    // Interfaces are mapped to AnyType (type constructor)
    assert(readerField.ty.isInstanceOf[AST.AnyType])
  }

  test("convert Go slice type to Chester ListType") {
    val json = """{
      "package": "test",
      "interfaces": [],
      "structs": [],
      "functions": [
        {
          "name": "Strings",
          "params": [],
          "results": [{"type": "[]string"}]
        }
      ]
    }"""

    val sig = GoToChester.packageSignature(json, "test")
    val funcField = sig.fields.head

    funcField.ty match
      case AST.Pi(_, returnType, _, _) =>
        returnType match
          case AST.ListType(elemType, _) =>
            assert(elemType.isInstanceOf[AST.StringType])
          case other => fail(s"Expected ListType for slice, got $other")
      case other => fail(s"Expected Pi type, got $other")
  }
}
