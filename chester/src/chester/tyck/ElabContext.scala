package chester.tyck

import scala.language.experimental.genericNumberLiterals
import scala.collection.mutable

import chester.core.{AST, Arg, BuiltinEffect, CST, Coeffect, EffectRef, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.error.{Problem, Reporter, Span, VectorReporter}
import chester.uniqid.{Uniqid, UniqidOf}
import chester.utils.elab.*
import chester.utils.{HoldNotReadable, given}
import chester.utils.doc.{<>, Doc, DocConf, DocOps, StringPrinter, ToDoc, given}
import chester.tyck.ASTOps.normalizeType
import cats.data.NonEmptyVector
/** Elaboration context tracking bindings and types during CST to AST conversion */
case class ElabContext(
    bindings: Map[String, UniqidOf[AST]], // Name -> variable ID mapping
    types: Map[UniqidOf[AST], CellRW[AST]], // Variable ID -> type cell mapping
    defBodies: Map[UniqidOf[AST], CellRW[AST]] = Map.empty, // Definition ID -> AST cell mapping
    effects: Map[String, EffectRef] = ElabContext.defaultEffects, // Declared effects
    jsImports: Map[String, JSImportSignature] = Map.empty, // JSImport (module path) -> signature
    recordsByName: Map[String, ElabContext.RecordDef] = Map.empty,
    recordsById: Map[UniqidOf[AST], ElabContext.RecordDef] = Map.empty,
    enumsByName: Map[String, ElabContext.EnumDef] = Map.empty,
    enumsById: Map[UniqidOf[AST], ElabContext.EnumDef] = Map.empty,
    builtins: Set[String] = ElabContext.defaultBuiltins, // Built-in names
    builtinTypes: Map[String, AST] = ElabContext.defaultBuiltinTypes, // Built-in types
    extensionBindings: Map[String, Set[UniqidOf[AST]]] = Map.empty, // Track extension methods
    reporter: Reporter[ElabProblem] // Error reporter
):
  def bind(name: String, id: UniqidOf[AST], ty: CellRW[AST]): ElabContext =
    copy(bindings = bindings + (name -> id), types = types + (id -> ty))

  def registerDefBody(id: UniqidOf[AST], cell: CellRW[AST]): ElabContext =
    copy(defBodies = defBodies + (id -> cell))

  def lookup(name: String): Option[UniqidOf[AST]] = bindings.get(name)
  def lookupType(id: UniqidOf[AST]): Option[CellRW[AST]] = types.get(id)
  def lookupDefBody(id: UniqidOf[AST]): Option[CellRW[AST]] = defBodies.get(id)
  def isBuiltin(name: String): Boolean = builtins.contains(name)
  def lookupBuiltinType(name: String): Option[AST] = builtinTypes.get(name)
  def registerRecordPlaceholder(name: String, id: UniqidOf[AST], ctorType: CellRW[AST]): ElabContext = {
    val defn = ElabContext.RecordDef(id, name, Vector.empty, ctorType)
    copy(
      recordsByName = recordsByName + (name -> defn),
      recordsById = recordsById + (id -> defn),
      bindings = bindings + (name -> id),
      types = types + (id -> ctorType)
    )
  }
  def updateRecord(name: String, fields: Vector[Param]): ElabContext = {
    recordsByName.get(name) match
      case Some(defn) =>
        val updated = defn.copy(fields = fields)
        copy(recordsByName = recordsByName + (name -> updated), recordsById = recordsById + (defn.id -> updated))
      case None => this
  }
  def lookupRecord(name: String): Option[ElabContext.RecordDef] = recordsByName.get(name)
  def lookupRecordById(id: UniqidOf[AST]): Option[ElabContext.RecordDef] = recordsById.get(id)
  def registerEnumPlaceholder(name: String, id: UniqidOf[AST], typeParams: Vector[Param], isCoinductive: Boolean): ElabContext = {
    val defn = ElabContext.EnumDef(id, name, typeParams, Vector.empty, isCoinductive = isCoinductive)
    copy(
      enumsByName = enumsByName + (name -> defn),
      enumsById = enumsById + (id -> defn)
    )
  }
  def updateEnum(name: String, typeParams: Vector[Param], cases: Vector[EnumCase], isCoinductive: Boolean): ElabContext = {
    enumsByName.get(name) match
      case Some(defn) =>
        val updated = defn.copy(typeParams = typeParams, cases = cases, isCoinductive = isCoinductive)
        copy(enumsByName = enumsByName + (name -> updated), enumsById = enumsById + (defn.id -> updated))
      case None => this
  }
  def lookupEnum(name: String): Option[ElabContext.EnumDef] = enumsByName.get(name)
  def lookupEnumById(id: UniqidOf[AST]): Option[ElabContext.EnumDef] = enumsById.get(id)
  def lookupEnumCase(enumId: UniqidOf[AST], caseName: String): Option[EnumCase] =
    enumsById.get(enumId).flatMap(_.cases.find(_.name == caseName))
  def declareEffect(name: String): ElabContext = {
    val eff = EffectRef.User(Uniqid.make, name)
    copy(effects = effects + (name -> eff))
  }
  def lookupEffect(name: String): Option[EffectRef] = effects.get(name)

object ElabContext:
  case class RecordDef(id: UniqidOf[AST], name: String, fields: Vector[Param], ctorType: CellRW[AST])
  case class EnumDef(id: UniqidOf[AST], name: String, typeParams: Vector[Param], cases: Vector[EnumCase], isCoinductive: Boolean)

  val defaultEffects: Map[String, EffectRef] = Map("io" -> EffectRef.Builtin(BuiltinEffect.Io))
  val defaultBuiltins: Set[String] = Set(
    "Integer",
    "Int",
    "String",
    "Bool",
    "Nat",
    "Natural",
    "Type",
    "Level",
    "U",
    "Universe",
    "+",
    "-",
    "true",
    "false",
    "List",
    "println"
  )

  val defaultBuiltinTypes: Map[String, AST] = {
    val stringParam = Param(Uniqid.make, "value", AST.StringType(None), Implicitness.Explicit, None)
    val tele = Vector(Telescope(Vector(stringParam), Implicitness.Explicit))
    val unitTy = AST.TupleType(Vector.empty, None)
    val printlnTy = AST.Pi(tele, unitTy, Vector(ElabContext.defaultEffects("io")), None)
    val intParam = Param(Uniqid.make, "x", AST.IntegerType(None), Implicitness.Explicit, None)
    val intParam2 = Param(Uniqid.make, "y", AST.IntegerType(None), Implicitness.Explicit, None)
    val intTele = Vector(Telescope(Vector(intParam, intParam2), Implicitness.Explicit))
    val plusTy = AST.Pi(intTele, AST.IntegerType(None), Vector.empty, None)
    val minusTy = AST.Pi(intTele, AST.IntegerType(None), Vector.empty, None)
    Map(
      "true" -> AST.BoolType(None),
      "false" -> AST.BoolType(None),
      "println" -> printlnTy,
      "+" -> plusTy,
      "-" -> minusTy,
      "Level" -> AST.Type(AST.LevelLit(0, None), None)
    )
  }

