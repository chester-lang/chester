package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.utils.asInt

class SymbolIndexTest extends munit.FunSuite:

  test("SymbolIndex finds def usages and resolves position") {
    val code = "{ def id(x: Integer) = x; id(1) }"
    val (astOpt, _, errors) = ElabTestUtils.elaborateFile(code)
    assert(errors.isEmpty, s"Expected no elaboration errors, got: $errors")
    val ast = astOpt.getOrElse(fail("Expected AST"))

    val index = SymbolIndex.fromAst(ast)
    val defId = index.definitions.collectFirst { case (id, entry) if entry.name == "id" => id }.getOrElse {
      fail("Expected to find definition for id")
    }

    val usages = index.usages(defId)
    assert(usages.nonEmpty, "Expected to record a usage of id")

    val firstUsageStart = usages.head.range.start
    val found =
      index.findSymbolAt(firstUsageStart.line.asInt, firstUsageStart.column.utf16.asInt)

    assertEquals(found, Some(defId))
  }

  test("SymbolIndex tracks record constructor reference") {
    val code = "{ record Box(value: Integer); Box(1) }"
    val (astOpt, _, errors) = ElabTestUtils.elaborateFile(code)
    assert(errors.isEmpty, s"Expected no elaboration errors, got: $errors")
    val ast = astOpt.getOrElse(fail("Expected AST"))

    val index = SymbolIndex.fromAst(ast)
    val recId = index.definitions.collectFirst { case (id, entry) if entry.name == "Box" => id }.getOrElse {
      fail("Expected record definition for Box")
    }

    val usages = index.usages(recId)
    assert(usages.nonEmpty, "Expected a usage span for Box constructor")

    val usageStart = usages.head.range.start
    val found = index.findSymbolAt(usageStart.line.asInt, usageStart.column.utf16.asInt)

    assertEquals(found, Some(recId))
  }
