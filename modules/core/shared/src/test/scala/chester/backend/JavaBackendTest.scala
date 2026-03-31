package chester.backend

import munit.FunSuite

import java.nio.file.Files

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.experimental.genericNumberLiterals
import scala.sys.process.*

import chester.tyck.ElabTestUtils
import chester.tyck.ElabTestUtils.{defaultTimeout, runAsync, given ExecutionContext}
import chester.utils.doc.{DocConf, render, *, given}

class JavaBackendTest extends FunSuite:

  private given DocConf = DocConf.Default

  override val munitTimeout: FiniteDuration = defaultTimeout

  private def normalize(output: String): String =
    output.linesIterator.map(_.trim).filter(_.nonEmpty).mkString("\n").trim

  private def compileWithJavac(rendered: String, className: String = "Main"): Unit = {
    val tempDir = Files.createTempDirectory("chester-java-test")
    val sourcePath = tempDir.resolve(s"$className.java")
    Files.writeString(sourcePath, rendered)
    val exitCode = Process(Seq("javac", sourcePath.getFileName.toString), tempDir.toFile).!
    assertEquals(exitCode, 0, s"Expected generated Java to compile:\n$rendered")
  }

  test("backend renders a simple Java program shape") {
    runAsync {
      val code =
        """{ record Box(value: String);
          |  def add1(x: Integer) = x + 1;
          |  add1(41) }""".stripMargin

      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")

      val unit = JavaBackend.lowerProgram(astOpt.get)
      val rendered = normalize(render(unit.toDoc).toString)

      assert(rendered.contains("public static final class Box"), s"Expected nested class for record, got:\n$rendered")
      assert(rendered.contains("public static Object add1(int x)"), s"Expected static method for def, got:\n$rendered")
      assert(rendered.contains("System.out.println(add1(41));"), s"Expected top-level expression to print, got:\n$rendered")
    }
  }

  test("record constructor lowering preserves field names") {
    runAsync {
      val code =
        """{ record Vec2d(x: Integer, y: Integer);
          |  let v: Vec2d.t = Vec2d(1, 2);
          |  v.x }""".stripMargin

      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")

      val unit = JavaBackend.lowerProgram(astOpt.get)
      val rendered = normalize(render(unit.toDoc).toString)

      assert(rendered.contains("public final int x;"), s"Expected named field x, got:\n$rendered")
      assert(rendered.contains("public Vec2d(int x, int y)"), s"Expected Java constructor, got:\n$rendered")
      assert(rendered.contains("new Vec2d(1, 2)"), s"Expected named record construction, got:\n$rendered")
      assert(rendered.contains("System.out.println(v.x);"), s"Expected field access to remain named, got:\n$rendered")
    }
  }

  test("Bool lowers to Java boolean") {
    runAsync {
      val code = """{ def negate(flag: Bool): Bool = flag; negate(false) }"""
      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")

      val unit = JavaBackend.lowerProgram(astOpt.get)
      val rendered = normalize(render(unit.toDoc).toString)

      assert(rendered.contains("public static boolean negate(boolean flag)"), s"Expected Bool to lower to boolean, got:\n$rendered")
      assert(rendered.contains("System.out.println(negate(false));"), s"Expected false literal to stay boolean, got:\n$rendered")
    }
  }

  test("generated Java for pure program compiles with javac") {
    runAsync {
      val code =
        """{ record Box(value: String);
          |  def add1(x: Integer): Integer = x + 1;
          |  let answer = add1(41);
          |  answer }""".stripMargin

      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")

      val unit = JavaBackend.lowerProgram(astOpt.get)
      val rendered = render(unit.toDoc).toString
      compileWithJavac(rendered)
    }
  }

  test("generated Java for Bool program compiles with javac") {
    runAsync {
      val code =
        """{ def negate(flag: Bool): Bool = flag;
          |  negate(false) }""".stripMargin

      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")

      val unit = JavaBackend.lowerProgram(astOpt.get)
      val rendered = render(unit.toDoc).toString
      compileWithJavac(rendered)
    }
  }

  test("handled effect program compiles after elaboration lowers helper defs") {
    runAsync {
      val code =
        """{
          |  effect magic { def ping(): Integer };
          |  handle { magic.ping() } with magic { def ping(): Integer = 42 }
          |}""".stripMargin

      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")

      val unit = JavaBackend.lowerProgram(astOpt.get)
      val rendered = normalize(render(unit.toDoc).toString)

      assert(rendered.contains("public static int ping()"), s"Expected handled effect helper to be hoisted as a Java method, got:\n$rendered")
      assert(rendered.contains("System.out.println"), s"Expected handled program to remain executable, got:\n$rendered")
      compileWithJavac(render(unit.toDoc).toString)
    }
  }

  test("effectful defs lower to CPS-shaped Java methods when enabled") {
    runAsync {
      val code =
        """{
          |  effect magic {};
          |  def answer(): Integer = 42;
          |  def foo(): Integer / [magic] = answer();
          |  ()
          |}""".stripMargin

      val (astOpt, _, errors) = ElabTestUtils.elaborateExpr(code)
      assert(errors.isEmpty, s"Elaboration failed: $errors")

      val unit = JavaBackend.lowerProgram(astOpt.get, config = JavaBackend.Config(applyEffectCPS = true))
      val rendered = normalize(render(unit.toDoc).toString)

      assert(rendered.contains("java.util.function.Function<Object, Object>"), s"Expected CPS continuation parameter type, got:\n$rendered")
      assert(rendered.contains("return k.apply(answer());"), s"Expected CPS body to call continuation, got:\n$rendered")
      compileWithJavac(render(unit.toDoc).toString)
    }
  }
