package chester.syntax

import scala.language.experimental.genericNumberLiterals

import chester.reader.{FileNameAndContent, Source}
import munit.FunSuite

class GoDeclParserTest extends FunSuite {
  test("parse grouped params and named results") {
    val goSource = """package sample

func Compare(a, b string, users []*User) (winner string, err error) {
    return "", nil
}
"""
    val sourceRef = Source(FileNameAndContent("sample.go", goSource))
    val parsed = GoDeclParser.parse(goSource, sourceRef)

    val func = parsed("functions").arr.head
    val params = func("params").arr
    val results = func("results").arr

    assertEquals(params.map(_("name").str).toVector, Vector("a", "b", "users"))
    assertEquals(params.map(_("type").str).toVector, Vector("string", "string", "[]*User"))
    assertEquals(results.map(_("name").str).toVector, Vector("winner", "err"))
    assertEquals(results.map(_("type").str).toVector, Vector("string", "error"))
  }

  test("parse interface method signatures") {
    val goSource = """package sample

type Reader interface {
    Read(p []byte) (n int, err error)
    Close() error
}
"""
    val sourceRef = Source(FileNameAndContent("reader.go", goSource))
    val parsed = GoDeclParser.parse(goSource, sourceRef)

    val iface = parsed("interfaces").arr.head
    val methods = iface("methods").arr
    val read = methods.find(_("name").str == "Read").getOrElse(fail("Read method missing"))
    val close = methods.find(_("name").str == "Close").getOrElse(fail("Close method missing"))

    assertEquals(read("params").arr.map(_("type").str).toVector, Vector("[]byte"))
    assertEquals(read("results").arr.map(_("type").str).toVector, Vector("int", "error"))
    assertEquals(close("params").arr.length, 0)
    assertEquals(close("results").arr.map(_("type").str).toVector, Vector("error"))
  }
}
