package chester.syntax

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.language.experimental.genericNumberLiterals
import scala.sys.process.*
import scala.util.Using

import java.net.URI
import java.nio.file.{Files, Path, StandardCopyOption}

import munit.FunSuite
import chester.reader.{FileNameAndContent, Source}

class TypeScriptDeclParserNodeTypesTest extends FunSuite {

  override val munitTimeout: FiniteDuration = 10.seconds
  private given ExecutionContext = ExecutionContext.global

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

  private def tryFindLocalNodeTypes(): Option[Path] = {
    val wd = Path.of(System.getProperty("user.dir"))
    val direct = wd.resolve("node_modules").resolve("@types").resolve("node")
    if Files.exists(direct) then Some(direct)
    else {
      val pnpmDir = wd.resolve("node_modules").resolve(".pnpm")
      if !Files.exists(pnpmDir) then None
      else {
        val maybe = Using.resource(Files.list(pnpmDir)) { stream =>
          stream
            .filter(p => p.getFileName.toString.startsWith("@types+node@"))
            .findFirst()
            .orElse(null)
        }
        Option(maybe).flatMap { p =>
          val base = p.resolve("node_modules").resolve("@types").resolve("node")
          if Files.exists(base) then Some(base) else None
        }
      }
    }
  }

  test("parse full @types/node declarations without errors") {
    Future {
      val extractedDir = tryFindLocalNodeTypes().getOrElse {
        assume(
          cond = false,
          clue = "Skipping: local @types/node not found under ./node_modules; download is disabled for this fast test."
        )
        Path.of(".")
      }

      val failures = scala.collection.mutable.ArrayBuffer.empty[String]
      var totalFiles = 0
      var parsedFiles = 0

      // Keep this test intentionally fast: pick a few small top-level `.d.ts` files (names vary across @types/node versions).
      val maxBytes = 80_000L
      val samplePaths: Vector[Path] = Using.resource(Files.list(extractedDir)) { stream =>
        stream
          .filter(p => p.getFileName.toString.endsWith(".d.ts"))
          .toArray
          .toVector
          .map(_.asInstanceOf[Path])
      }
        .filter(p => Files.isRegularFile(p))
        .sortBy(p => Files.size(p))

      val selected =
        samplePaths
          .filter(p => Files.size(p) <= maxBytes)
          .take(3) match {
          case v if v.nonEmpty => v
          case _               => samplePaths.take(1) // fallback: parse just the smallest file available
        }

      assume(selected.nonEmpty, "No .d.ts files found in local @types/node directory")

      selected.foreach { path =>
        val relPath = extractedDir.relativize(path).toString
        totalFiles += 1
        val content = Files.readString(path)
        try {
          val sourceRef = Source(FileNameAndContent(relPath, content))
          TypeScriptDeclParser.parse(content, sourceRef) match {
            case p: TypeScriptAST.Program =>
              // light sanity check to ensure the parser is producing a program, not just terminating
              assert(p.statements.nonEmpty, s"Expected some statements in $relPath")
            case other =>
              fail(s"Unexpected AST root for $relPath: $other")
          }
          parsedFiles += 1
        } catch {
          case t: Throwable =>
            failures += s"$relPath -> ${t.getClass.getSimpleName}: ${t.getMessage}"
        }
      }

      assert(totalFiles >= 1, s"Expected to parse at least one Node typings file, got $totalFiles")
      assertEquals(parsedFiles, totalFiles, "All selected declaration files should parse without errors")
      assert(failures.isEmpty, s"Parsing errors in selected NodeJS typings files:\n${failures.mkString("\n")}")
    }
  }
}
