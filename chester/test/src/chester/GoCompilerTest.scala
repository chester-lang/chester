package chester

class GoCompilerTest extends munit.FunSuite {
  private def normalize(s: String): String =
    s.linesIterator.map(_.trim).filter(_.nonEmpty).mkString("\n").trim

  test("compile record and function definition") {
    val code =
      """record Box(value: String);
        |def add1(x: Integer): Integer = {
        |  x + 1
        |};
        |()
      """.stripMargin

    val compiled = GoCompiler.compile(code, "main")
    val expected = normalize(
      """package main
        |type Box struct {
        |  value string
        |}
        |func add1(x int) int {
        |  return x + 1
        |}
      """.stripMargin
    )
    assertEquals(normalize(compiled), expected)
  }

  test("compile let binding and print result") {
    val code =
      """let x = 42;
        |x
      """.stripMargin

    val compiled = GoCompiler.compile(code, "main")
    val expected = normalize(
      """package main
        |import "fmt"
        |func main() {
        |  x := 42
        |  fmt.Println(x)
        |}
      """.stripMargin
    )
    assertEquals(normalize(compiled), expected)
  }

  test("compile complex program with constructor and function call") {
    val code =
      """record Vec2d(x: Integer, y: Integer);
        |def addVec(v1: Vec2d, v2: Vec2d): Vec2d = {
        |  Vec2d(v1.x + v2.x, v1.y + v2.y)
        |};
        |addVec(Vec2d(1, 2), Vec2d(3, 4))
      """.stripMargin

    val compiled = GoCompiler.compile(code, "main")
    val expected = normalize(
      """package main
        |import "fmt"
        |type Vec2d struct {
        |  x int
        |  y int
        |}
        |func addVec(v1 Vec2d, v2 Vec2d) Vec2d {
        |  return Vec2d{v1.x + v2.x, v1.y + v2.y}
        |}
        |func main() {
        |  fmt.Println(addVec(Vec2d{1, 2}, Vec2d{3, 4}))
        |}
      """.stripMargin
    )
    assertEquals(normalize(compiled), expected)
  }
}
