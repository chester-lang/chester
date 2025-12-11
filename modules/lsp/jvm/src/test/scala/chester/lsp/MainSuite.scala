package chester.lsp

import scala.language.experimental.genericNumberLiterals

class MainSuite extends munit.FunSuite:
  test("DocumentAnalyzer indexes defs and records") {
    val code =
      """record Box(value: Integer);
        |def id(x: Integer) = x;
        |id(1)""".stripMargin

    val analysis = DocumentAnalyzer.analyze("file:///test.chester", code)
    analysis match
      case Left(errors) => fail(s"Expected analysis to succeed, got errors: $errors")
      case Right(result) =>
        val names = result.index.definitions.values.map(_.name).toSet
        assert(names.contains("Box"), s"Expected record definition, got: $names")
        assert(names.contains("id"), s"Expected def definition, got: $names")
  }
