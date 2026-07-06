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

object FFISignatures {
  def stringsSignature: GoImportSignature = {
    val sParam = Param(Uniqid.make, "s", AST.StringType(None), Implicitness.Explicit, None)
    val toUpperTy = AST.Pi(
      Vector(Telescope(Vector(sParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector.empty,
      None
    )
    GoImportSignature(
      Vector(
        Param(Uniqid.make, "ToUpper", toUpperTy, Implicitness.Explicit, None)
      ),
      "strings"
    )
  }

  def fmtSignature: GoImportSignature = {
    val formatParam = Param(Uniqid.make, "format", AST.StringType(None), Implicitness.Explicit, None)
    val argsParam = Param(Uniqid.make, "args", AST.ListType(AST.AnyType(None), None), Implicitness.Explicit, None)
    val printfTy = AST.Pi(
      Vector(Telescope(Vector(formatParam, argsParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector(EffectRef.Builtin(BuiltinEffect.Io)),
      None
    )
    val sprintfTy = AST.Pi(
      Vector(Telescope(Vector(formatParam, argsParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector.empty,
      None
    )
    val printlnTy = AST.Pi(
      Vector(Telescope(Vector(argsParam), Implicitness.Explicit)),
      AST.TupleType(Vector.empty, None),
      Vector(EffectRef.Builtin(BuiltinEffect.Io)),
      None
    )
    GoImportSignature(
      Vector(
        Param(Uniqid.make, "Printf", printfTy, Implicitness.Explicit, None),
        Param(Uniqid.make, "Sprintf", sprintfTy, Implicitness.Explicit, None),
        Param(Uniqid.make, "Println", printlnTy, Implicitness.Explicit, None)
      ),
      "fmt"
    )
  }

  def pathSignature: JSImportSignature = {
    val pathsParam = Param(Uniqid.make, "paths", AST.ListType(AST.StringType(None), None), Implicitness.Explicit, None)
    val joinTy = AST.Pi(
      Vector(Telescope(Vector(pathsParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector(EffectRef.Builtin(BuiltinEffect.Io)),
      None
    )
    JSImportSignature(
      Vector(
        Param(Uniqid.make, "join", joinTy, Implicitness.Explicit, None)
      ),
      chester.core.JSImportKind.Default
    )
  }

  def consoleSignature: JSImportSignature = {
    val logParam = Param(Uniqid.make, "message", AST.AnyType(None), Implicitness.Explicit, None)
    val logTy = AST.Pi(
      Vector(Telescope(Vector(logParam), Implicitness.Explicit)),
      AST.TupleType(Vector.empty, None),
      Vector(EffectRef.Builtin(BuiltinEffect.Io)),
      None
    )
    JSImportSignature(
      Vector(
        Param(Uniqid.make, "log", logTy, Implicitness.Explicit, None)
      ),
      chester.core.JSImportKind.Default
    )
  }

  def utilSignature: JSImportSignature = {
    val formatParam = Param(Uniqid.make, "format", AST.StringType(None), Implicitness.Explicit, None)
    val argsParam = Param(Uniqid.make, "args", AST.ListType(AST.AnyType(None), None), Implicitness.Explicit, None)
    val formatTy = AST.Pi(
      Vector(Telescope(Vector(formatParam, argsParam), Implicitness.Explicit)),
      AST.StringType(None),
      Vector.empty,
      None
    )
    JSImportSignature(
      Vector(
        Param(Uniqid.make, "format", formatTy, Implicitness.Explicit, None)
      ),
      chester.core.JSImportKind.Default
    )
  }


}
