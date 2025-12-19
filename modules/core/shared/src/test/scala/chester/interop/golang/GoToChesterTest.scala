package chester.interop.golang

import scala.language.experimental.genericNumberLiterals

import munit.FunSuite
import chester.core.{AST, Implicitness, Param}
import chester.tyck.GoImportSignature
import chester.reader.{FileNameAndContent, Source}

class GoToChesterTest extends FunSuite {

  // ========== JSON-based tests (legacy compatibility) ==========

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

  // ========== Go source parsing tests (NEW!) ==========

  test("parse Go function declaration from source") {
    val goSource = """package os

func Getwd() (string, error) {
    return "", nil
}

func Hostname() (name string, err error) {
    return "", nil
}
"""
    val source = Source(FileNameAndContent("os.go", goSource))
    val sig = GoToChester.fromGoSource(goSource, source, "os")

    assertEquals(sig.packageName, "os")
    assertEquals(sig.fields.length, 2)
    
    val getwd = sig.fields.find(_.name == "Getwd")
    assert(getwd.isDefined, "Should find Getwd function")
    
    val hostname = sig.fields.find(_.name == "Hostname")
    assert(hostname.isDefined, "Should find Hostname function")
  }

  test("parse Go struct from source") {
    val goSource = """package main

type Person struct {
    Name string
    Age  int
}

type Address struct {
    Street string
    City   string
}
"""
    val source = Source(FileNameAndContent("main.go", goSource))
    val sig = GoToChester.fromGoSource(goSource, source, "main")

    assertEquals(sig.fields.length, 2)
    
    val person = sig.fields.find(_.name == "Person")
    assert(person.isDefined, "Should find Person struct")
    assert(person.get.ty.isInstanceOf[AST.AnyType])
    
    val address = sig.fields.find(_.name == "Address")
    assert(address.isDefined, "Should find Address struct")
  }

  test("parse Go interface from source") {
    val goSource = """package io

type Reader interface {
    Read(p []byte) (n int, err error)
}

type Writer interface {
    Write(p []byte) (n int, err error)
}
"""
    val source = Source(FileNameAndContent("io.go", goSource))
    val sig = GoToChester.fromGoSource(goSource, source, "io")

    assertEquals(sig.fields.length, 2)
    
    val reader = sig.fields.find(_.name == "Reader")
    assert(reader.isDefined, "Should find Reader interface")
    
    val writer = sig.fields.find(_.name == "Writer")
    assert(writer.isDefined, "Should find Writer interface")
  }

  test("only export capitalized (public) declarations") {
    val goSource = """package main

func PublicFunc() string {
    return ""
}

func privateFunc() string {
    return ""
}

type PublicStruct struct {
    Field string
}

type privateStruct struct {
    field string
}
"""
    val source = Source(FileNameAndContent("main.go", goSource))
    val sig = GoToChester.fromGoSource(goSource, source, "main")

    // Should only have 2 exports: PublicFunc and PublicStruct
    assertEquals(sig.fields.length, 2)
    
    val names = sig.fields.map(_.name).toSet
    assert(names.contains("PublicFunc"))
    assert(names.contains("PublicStruct"))
    assert(!names.contains("privateFunc"))
    assert(!names.contains("privateStruct"))
  }

  test("handle Go comments and whitespace") {
    val goSource = """package main

// Greet returns a greeting message
func Greet(name string) string {
    return "Hello, " + name
}

/*
 Block comment
 Multiple lines
*/
type Config struct {
    // Field comment
    Host string
}
"""
    val source = Source(FileNameAndContent("main.go", goSource))
    val sig = GoToChester.fromGoSource(goSource, source, "main")

    assertEquals(sig.fields.length, 2)
    val names = sig.fields.map(_.name).toSet
    assert(names.contains("Greet"))
    assert(names.contains("Config"))
  }

  test("parse complex Go types") {
    val goSource = """package main

type User struct {
    Name    string
    Tags    []string
    Manager *User
    Data    map[string]interface{}
}

func Process(users []*User) ([]*User, error) {
    return nil, nil
}
"""
    val source = Source(FileNameAndContent("main.go", goSource))
    val sig = GoToChester.fromGoSource(goSource, source, "main")

    assertEquals(sig.fields.length, 2)
    
    val user = sig.fields.find(_.name == "User")
    assert(user.isDefined, "Should find User struct")
    
    val process = sig.fields.find(_.name == "Process")
    assert(process.isDefined, "Should find Process function")
  }

  test("extract correct package name") {
    val goSource = """package mypackage

func Hello() string {
    return "world"
}
"""
    val source = Source(FileNameAndContent("test.go", goSource))
    val sig = GoToChester.fromGoSource(goSource, source, "mypackage")

    assertEquals(sig.packageName, "mypackage")
  }

  test("handle empty Go source") {
    val goSource = """package main
"""
    val source = Source(FileNameAndContent("empty.go", goSource))
    val sig = GoToChester.fromGoSource(goSource, source, "main")

    assertEquals(sig.packageName, "main")
    assertEquals(sig.fields.length, 0)
  }
}
