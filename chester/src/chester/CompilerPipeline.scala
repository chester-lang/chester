package chester

import chester.core.{AST, Param, Telescope, BuiltinEffect, EffectRef, Implicitness}
import chester.tyck.{ElabConstraint, ElabContext, ElabHandlerConf, ElabProblem, GoImportSignature, JSImportSignature, CoreTypeChecker, substituteSolutions}
import chester.uniqid.Uniqid
import chester.utils.elab.ProceduralSolverModule
import chester.error.VectorReporter
import chester.backend.GoBackend
import chester.backend.TypeScriptBackend
import chester.backend.JavaBackend
import chester.syntax.TypeScriptAST
import chester.syntax.JavaAST
import java.nio.file.{Paths, Files}
import scala.sys.process.*
import chester.utils.doc.{DocConf, render}

object CompilerPipeline {
  def loadStdlib(target: String): String = {
    import java.io.File
    import scala.io.Source

    def loadDir(path: String, localFallback: String): String = {
      val url = getClass.getResource(path)
      if (url != null && url.getProtocol == "file") {
        val dir = new File(url.toURI)
        if (dir.exists() && dir.isDirectory) {
          dir.listFiles()
            .filter(f => f.isFile && f.getName.endsWith(".chester"))
            .map(f => Files.readString(f.toPath))
            .mkString("\n")
        } else ""
      } else {
        val localDir = new File(localFallback)
        if (localDir.exists() && localDir.isDirectory) {
          localDir.listFiles()
            .filter(f => f.isFile && f.getName.endsWith(".chester"))
            .map(f => Files.readString(f.toPath))
            .mkString("\n")
        } else ""
      }
    }

    val core = loadDir("/stdlib", "chester/resources/stdlib")
    val targetSpecific = loadDir(s"/stdlib/$target", s"chester/resources/stdlib/$target")

    (core + "\n" + targetSpecific).trim
  }

  def elaborate(content: String): (Option[AST], Option[AST], Vector[ElabProblem]) = {
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val source = Source(FileNameAndContent("file.chester", content))
    val parsed = for
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    yield {
      val cst = Parser.parseFile(tokens)

      val module = ProceduralSolverModule
      val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))
      val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
      val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

      val stdGoImports = Map("fmt" -> FFISignatures.fmtSignature, "strings" -> FFISignatures.stringsSignature)
      val stdJsImports = Map("path" -> FFISignatures.pathSignature, "console" -> FFISignatures.consoleSignature, "util" -> FFISignatures.utilSignature)
      val allImports = (stdGoImports.map { case (k, v) => k -> JSImportSignature(v.fields) } ++ stdJsImports).toMap
      println(s"allImports keys: ${allImports.keys}")

      val tId_length = Uniqid.make[AST]
      val listLengthTy = AST.Pi(
        Vector(
          Telescope(Vector(Param(tId_length, "T", AST.Type(AST.LevelLit(0, None), None), Implicitness.Implicit, None, chester.core.Coeffect.Zero)), Implicitness.Implicit),
          Telescope(Vector(Param(Uniqid.make, "list", AST.ListType(AST.Ref(tId_length, "T", None), None), Implicitness.Explicit, None)), Implicitness.Explicit)
        ),
        AST.IntegerType(None),
        Vector.empty,
        None
      )

      val tId_get = Uniqid.make[AST]
      val listGetTy = AST.Pi(
        Vector(
          Telescope(Vector(Param(tId_get, "T", AST.Type(AST.LevelLit(0, None), None), Implicitness.Implicit, None, chester.core.Coeffect.Zero)), Implicitness.Implicit),
          Telescope(Vector(
            Param(Uniqid.make, "list", AST.ListType(AST.Ref(tId_get, "T", None), None), Implicitness.Explicit, None),
            Param(Uniqid.make, "index", AST.IntegerType(None), Implicitness.Explicit, None)
          ), Implicitness.Explicit)
        ),
        AST.Ref(tId_get, "T", None),
        Vector.empty,
        None
      )

      val tId_make = Uniqid.make[AST]
      val genParam = Param(Uniqid.make, "i", AST.IntegerType(None), Implicitness.Explicit, None)
      val generatorTy = AST.Pi(
        Vector(Telescope(Vector(genParam), Implicitness.Explicit)),
        AST.Ref(tId_make, "T", None),
        Vector.empty,
        None
      )
      val listMakeTy = AST.Pi(
        Vector(
          Telescope(Vector(Param(tId_make, "T", AST.Type(AST.LevelLit(0, None), None), Implicitness.Implicit, None, chester.core.Coeffect.Zero)), Implicitness.Implicit),
          Telescope(Vector(
            Param(Uniqid.make, "size", AST.IntegerType(None), Implicitness.Explicit, None),
            Param(Uniqid.make, "generator", generatorTy, Implicitness.Explicit, None)
          ), Implicitness.Explicit)
        ),
        AST.ListType(AST.Ref(tId_make, "T", None), None),
        Vector.empty,
        None
      )

      val tId_if = Uniqid.make[AST]
      val ifElseTy = AST.Pi(
        Vector(
          Telescope(Vector(Param(tId_if, "T", AST.Type(AST.LevelLit(0, None), None), Implicitness.Implicit, None, chester.core.Coeffect.Zero)), Implicitness.Implicit),
          Telescope(Vector(
            Param(Uniqid.make, "cond", AST.BoolType(None), Implicitness.Explicit, None),
            Param(Uniqid.make, "thenVal", AST.Ref(tId_if, "T", None), Implicitness.Explicit, None),
            Param(Uniqid.make, "elseVal", AST.Ref(tId_if, "T", None), Implicitness.Explicit, None)
          ), Implicitness.Explicit)
        ),
        AST.Ref(tId_if, "T", None),
        Vector.empty,
        None
      )

      val intEqTy = AST.Pi(
        Vector(Telescope(Vector(
          Param(Uniqid.make, "a", AST.IntegerType(None), Implicitness.Explicit, None),
          Param(Uniqid.make, "b", AST.IntegerType(None), Implicitness.Explicit, None)
        ), Implicitness.Explicit)),
        AST.BoolType(None),
        Vector.empty,
        None
      )

      val intLtTy = AST.Pi(
        Vector(Telescope(Vector(
          Param(Uniqid.make, "a", AST.IntegerType(None), Implicitness.Explicit, None),
          Param(Uniqid.make, "b", AST.IntegerType(None), Implicitness.Explicit, None)
        ), Implicitness.Explicit)),
        AST.BoolType(None),
        Vector.empty,
        None
      )

      val id_length = Uniqid.make[AST]
      val cell_length = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_length, listLengthTy)

      val id_get = Uniqid.make[AST]
      val cell_get = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_get, listGetTy)

      val id_make = Uniqid.make[AST]
      val cell_make = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_make, listMakeTy)

      val id_if = Uniqid.make[AST]
      val cell_if = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_if, ifElseTy)

      val id_eq = Uniqid.make[AST]
      val cell_eq = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_eq, intEqTy)

      val id_lt = Uniqid.make[AST]
      val cell_lt = module.newOnceCell[ElabConstraint, AST](solver)
      module.fill(solver, cell_lt, intLtTy)

      val initialBindings = Map(
        "prim__list_length" -> id_length,
        "prim__list_get" -> id_get,
        "prim__list_make" -> id_make,
        "prim__if_else" -> id_if,
        "prim__int_eq" -> id_eq,
        "prim__int_lt" -> id_lt
      )
      val initialTypes = Map(
        id_length -> cell_length,
        id_get -> cell_get,
        id_make -> cell_make,
        id_if -> cell_if,
        id_eq -> cell_eq,
        id_lt -> cell_lt
      )

      val ctx = ElabContext(bindings = initialBindings, types = initialTypes, jsImports = allImports, reporter = elabReporter)

      module.addConstraint(solver, ElabConstraint.InferTopLevel(cst, resultCell, typeCell, ctx))
      module.run(solver)

      val result = module.readStable(solver, resultCell)
      val ty = module.readStable(solver, typeCell)
      val zonkedResult = result.map(r => substituteSolutions(r)(using module, solver))
      val zonkedTy = ty.map(t => substituteSolutions(t)(using module, solver))

      if (elabReporter.getReports.isEmpty && zonkedResult.isDefined) {
        val solvedInitialTypes = initialTypes.flatMap { case (id, cell) =>
          module.readStable(solver, cell).map(substituteSolutions(_)(using module, solver)).map(id -> _)
        }
        CoreTypeChecker.typeCheck(zonkedResult.get, solvedInitialTypes)(using elabReporter)
        chester.tyck.UsageChecker.checkUsage(zonkedResult.get)(using elabReporter)
      }

      (zonkedResult, zonkedTy)
    }

    parsed match
      case Right((ast, ty)) => (ast, ty, elabReporter.getReports)
      case Left(err)        => (None, None, Vector(ElabProblem.UnboundVariable(err.toString, None)))
  }


}
