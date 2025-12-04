package chester.core

class GreeterTest extends munit.FunSuite {
  test("greet should return a greeting message") {
    val result = Greeter.greet("World")
    assert(result.contains("Hello, World"))
  }
  
  test("platform should return platform name") {
    val result = Greeter.platform
    assert(result.nonEmpty)
  }
}
