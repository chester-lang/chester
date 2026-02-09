package chester.cli

import scala.annotation.experimental
import scala.language.experimental.genericNumberLiterals

import cats.Id
import chester.utils.io.impl.given
import chester.utils.term.Terminal
import munit.FunSuite

@experimental
class ExampleIntegrationTest extends FunSuite:

  private given Terminal[Id] = chester.utils.io.DefaultTerminal

  private def findRepoRoot(start: os.Path): os.Path =
    var cur = start
    var i = 0
    while i < 20 && !os.exists(cur / "build.sbt") do
      cur = cur / os.up
      i += 1
    assert(os.exists(cur / "build.sbt"), s"Could not find repo root from $start")
    cur

  private val repoRoot: os.Path = findRepoRoot(os.pwd)
  private val tsExamples: os.Path = repoRoot / "examples" / "ts"
  private val goExamples: os.Path = repoRoot / "examples" / "go"
  private val goSigs: os.Path = repoRoot / "go-signatures.json"
  private val pureTsExamples: Vector[String] = Vector("ts-simple", "ts-wordcount")

  private def commandExists(cmd: String): Boolean =
    os.proc("sh", "-c", s"command -v $cmd").call(check = false).exitCode == 0

  private def compareGenerated(srcDir: os.Path, outDir: os.Path, ext: String): Unit =
    val sources = os.list(srcDir).filter(p => p.ext == "chester").sortBy(_.last)
    assert(sources.nonEmpty, s"No .chester files found in $srcDir")

    sources.foreach { src =>
      val base = src.baseName
      val expected = srcDir / s"$base.$ext"
      val actual = outDir / s"$base.$ext"
      assert(os.exists(actual), s"Missing generated file: $actual")
      assert(os.exists(expected), s"Missing expected checked-in file: $expected")
      val exp = os.read(expected)
      val act = os.read(actual)
      assertEquals(act, exp, clue = s"Generated output differs for $base.$ext")
    }

  private def compilePureTsExamples(outDir: os.Path): Unit =
    pureTsExamples.foreach { base =>
      val src = tsExamples / s"$base.chester"
      CLI.run[Id](Config.CompileTS(src.toString, Some(outDir.toString)))
    }

  private def compareGeneratedSubset(srcDir: os.Path, outDir: os.Path, ext: String, bases: Vector[String]): Unit =
    bases.foreach { base =>
      val expected = srcDir / s"$base.$ext"
      val actual = outDir / s"$base.$ext"
      assert(os.exists(actual), s"Missing generated file: $actual")
      assert(os.exists(expected), s"Missing expected checked-in file: $expected")
      val exp = os.read(expected)
      val act = os.read(actual)
      assertEquals(act, exp, clue = s"Generated output differs for $base.$ext")
    }

  test("TypeScript pure examples: compile and match checked-in outputs") {
    val outDir = os.temp.dir(prefix = "chester-ts-examples-")
    compilePureTsExamples(outDir)
    compareGeneratedSubset(tsExamples, outDir, "ts", pureTsExamples)
  }

  test("TypeScript pure examples: generated files execute in node") {
    assume(commandExists("node"), "Skipping: node is not available in PATH")
    val outDir = os.temp.dir(prefix = "chester-ts-run-")
    compilePureTsExamples(outDir)

    val generated = pureTsExamples.map(base => outDir / s"$base.ts")
    assert(generated.nonEmpty, s"No generated .ts files in $outDir")
    generated.foreach { file =>
      val run = os.proc("node", file.toString).call(cwd = outDir, check = false, mergeErrIntoOut = true)
      assertEquals(run.exitCode, 0, clue = s"node failed for $file:\n${run.out.text()}")
    }
  }

  test("Go examples: compile and match checked-in outputs") {
    val outDir = os.temp.dir(prefix = "chester-go-examples-")
    CLI.run[Id](Config.CompileGo(goExamples.toString, Some(outDir.toString), Some(goSigs.toString)))
    compareGenerated(goExamples, outDir, "go")
  }

  test("Go examples: generated files build and binaries run") {
    assume(commandExists("go"), "Skipping: go toolchain is not available in PATH")
    val outDir = os.temp.dir(prefix = "chester-go-run-")
    CLI.run[Id](Config.CompileGo(goExamples.toString, Some(outDir.toString), Some(goSigs.toString)))

    val generated = os.list(outDir).filter(_.ext == "go").sortBy(_.last)
    assert(generated.nonEmpty, s"No generated .go files in $outDir")

    generated.foreach { file =>
      val bin = outDir / s"${file.baseName}.bin"
      val build = os.proc("go", "build", "-o", bin.toString, file.toString).call(cwd = outDir, check = false, mergeErrIntoOut = true)
      assertEquals(build.exitCode, 0, clue = s"go build failed for $file:\n${build.out.text()}")

      val run = os.proc(bin.toString).call(cwd = outDir, check = false, mergeErrIntoOut = true)
      assertEquals(run.exitCode, 0, clue = s"binary execution failed for $file:\n${run.out.text()}")
    }
  }
