package chester.tyck

import munit.FunSuite

class EffectTest extends FunSuite {
  test("State effect elaboration") {
    val code = """
      |effect State {
      |  def get(): Integer;
      |  def put(x: Integer): Unit
      |};
      |
      |def main(): Unit = {
      |  handle {
      |    let x = do get();
      |    do put(x + 1)
      |  } with State {
      |    def get() = 42;
      |    def put(x) = { }
      |  }
      |};
      |""".stripMargin
    val (_, _, problems) = ElabTestUtils.elaborateFile(code)
    assertEquals(problems, Vector.empty)
  }

  test("Exception effect elaboration") {
    val code = """
      |effect Exception {
      |  def raise(msg: String): Any
      |};
      |
      |def divide(): Integer / [Exception] = {
      |  do raise("Division by zero")
      |};
      |
      |def main(): Integer = {
      |  handle {
      |    divide()
      |  } with Exception {
      |    def raise(msg) = 0
      |  }
      |};
      |""".stripMargin
    val (_, _, problems) = ElabTestUtils.elaborateFile(code)
    assertEquals(problems, Vector.empty)
  }

  test("Yield effect elaboration") {
    val code = """
      |effect Yield {
      |  def yield(value: Integer): Unit
      |};
      |
      |def generate(): Unit / [Yield] = {
      |  do yield(1);
      |  do yield(2)
      |};
      |
      |def main(): Unit = {
      |  handle {
      |    generate()
      |  } with Yield {
      |    def yield(v) = { }
      |  }
      |};
      |""".stripMargin
    val (_, _, problems) = ElabTestUtils.elaborateFile(code)
    assertEquals(problems, Vector.empty)
  }
}
