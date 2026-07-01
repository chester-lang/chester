package chester

import scala.language.experimental.genericNumberLiterals

class GoDeclParserTest extends munit.FunSuite {
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

  test("parse method with receiver") {
    val goSource = """package sample
    
func (u *User) GetFullName(prefix string) (fullName string) {
    return prefix + u.Name, nil
}
"""
    val sourceRef = Source(FileNameAndContent("user.go", goSource))
    val parsed = GoDeclParser.parse(goSource, sourceRef)

    val func = parsed("functions").arr.head
    val receiver = func("receiver").obj
    assertEquals(receiver("name").str, "u")
    assertEquals(receiver("type").str, "*User")
    assertEquals(func("name").str, "GetFullName")
  }

  test("parse struct with embedded and tagged fields") {
    val goSource = """package sample

type Employee struct {
    Person
    *Address
    Salary int `json:"salary"`
    Bonus, Tax float64 `json:"fin"`
}
"""
    val sourceRef = Source(FileNameAndContent("employee.go", goSource))
    val parsed = GoDeclParser.parse(goSource, sourceRef)

    val struct = parsed("structs").arr.head
    val fields = struct("fields").arr

    assertEquals(fields.map(_("name").str).toVector, Vector("Person", "Address", "Salary", "Bonus", "Tax"))
    assertEquals(fields.map(_("type").str).toVector, Vector("Person", "*Address", "int", "float64", "float64"))
  }

  test("parse complex Go type expressions") {
    val goSource = """package sample

type Config struct {
    Metadata map[string]struct{ Name string }
    Channel  chan<- *Event
    Callback func(a int, b string) (bool, error)
}
"""
    val sourceRef = Source(FileNameAndContent("config.go", goSource))
    val parsed = GoDeclParser.parse(goSource, sourceRef)

    val struct = parsed("structs").arr.head
    val fields = struct("fields").arr

    assertEquals(fields.map(_("name").str).toVector, Vector("Metadata", "Channel", "Callback"))
    assertEquals(fields(0)("type").str, "map[string]struct{Name string}")
    assertEquals(fields(1)("type").str, "chan<-*Event")
    assertEquals(fields(2)("type").str, "func(a int,b string)(bool,error)")
  }
}
