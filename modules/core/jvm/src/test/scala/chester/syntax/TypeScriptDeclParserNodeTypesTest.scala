package chester.syntax

import scala.language.experimental.genericNumberLiterals
import scala.sys.process.*
import scala.util.Using

import java.net.URI
import java.nio.file.{Files, Path, StandardCopyOption}

import munit.FunSuite
import chester.reader.{FileNameAndContent, Source}

class TypeScriptDeclParserNodeTypesTest extends FunSuite {

  private val nodeTypesVersion = "22.7.0"
  private val nodeTypesTarball = s"https://registry.npmjs.org/@types/node/-/node-$nodeTypesVersion.tgz"

  /** Download and extract the @types/node tarball into a temp directory. */
  private def downloadNodeTypes(): Path = {
    val tmpDir = Files.createTempDirectory("node-types-")
    tmpDir.toFile.deleteOnExit()

    val tarballPath = tmpDir.resolve("node-types.tgz")
    Using.resource(URI.create(nodeTypesTarball).toURL.openStream()) { in =>
      Files.copy(in, tarballPath, StandardCopyOption.REPLACE_EXISTING)
    }

    val exit = Process(Seq("tar", "-xzf", tarballPath.toString, "-C", tmpDir.toString)).!
    assertEquals(exit, 0, s"tar extraction failed with exit code $exit")

    List("package", "node")
      .map(tmpDir.resolve)
      .find(Files.exists(_))
      .getOrElse(fail("Could not locate extracted @types/node directory"))
  }

  test("parse full @types/node declarations without errors") {
    val extractedDir = downloadNodeTypes()

    val failures = scala.collection.mutable.ArrayBuffer.empty[String]
    var totalFiles = 0
    var parsedFiles = 0

    Using.resource(Files.walk(extractedDir)) { stream =>
      val it = stream.iterator()
      while (it.hasNext) {
        val path = it.next()
        if (Files.isRegularFile(path) && path.toString.endsWith(".d.ts")) {
          totalFiles += 1
          val content = Files.readString(path)
          try {
            val sourceRef = Source(FileNameAndContent(extractedDir.relativize(path).toString, content))
            TypeScriptDeclParser.parse(content, sourceRef)
            parsedFiles += 1
          } catch {
            case t: Throwable =>
              failures += s"${extractedDir.relativize(path)} -> ${t.getClass.getSimpleName}: ${t.getMessage}"
          }
        }
      }
    }

    assert(
      totalFiles >= 40 && totalFiles <= 200,
      s"Unexpected .d.ts file count, expected Node typings magnitude, got $totalFiles"
    )
    assertEquals(parsedFiles, totalFiles, "All declaration files should parse without errors")
    assert(failures.isEmpty, s"Parsing errors in NodeJS typings:\n${failures.mkString("\n")}")

    def parseRelative(relPath: String): TypeScriptAST.Program = {
      val target = extractedDir.resolve(relPath)
      assert(Files.exists(target), s"Expected file to exist: $relPath")
      val content = Files.readString(target)
      val sourceRef = Source(FileNameAndContent(relPath, content))
      TypeScriptDeclParser.parse(content, sourceRef) match {
        case p: TypeScriptAST.Program => p
        case other                    => fail(s"Unexpected AST root for $relPath: $other")
      }
    }

    // Spot check a couple of files for sensible parse results.
    val globalsGlobal = parseRelative("globals.global.d.ts")
    val hasGlobalVar = globalsGlobal.statements.exists {
      case TypeScriptAST.VariableDeclaration(_, decls, _) => decls.exists(_.name == "global")
      case _                                              => false
    }
    assert(hasGlobalVar, "globals.global.d.ts should expose a declare var global")

    val globals = parseRelative("globals.d.ts")
    val aliasNames = globals.statements.collect { case TypeScriptAST.TypeAliasDeclaration(name, _, _, _) => name }.toSet
    assert(aliasNames.contains("_Request"), "globals.d.ts should contain the _Request type alias")
  }
}
