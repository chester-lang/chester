// TODO: More correctly implement toDoc
package chester.syntax.core

import chester.doc.const.{ColorProfile, Docs}
import chester.doc.*
import chester.doc.Doc.group
import chester.error.*
import chester.syntax.{Builtin, Id, QualifiedIDString}
import chester.utils.encodeString
import spire.math.Rational

import scala.language.implicitConversions

case class TermMeta(sourcePos: Option[SourcePos])

// so that it is not used in compare
case class OptionTermMeta(meta: Option[TermMeta] = None) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case _: OptionTermMeta => true
      case _ => false
    }
  }
}

implicit def toOptionTermMeta(meta: Option[TermMeta]): OptionTermMeta = new OptionTermMeta(meta)
implicit def fromOptionTermMeta(meta: OptionTermMeta): Option[TermMeta] = meta.meta

sealed trait TermWithMeta extends Term with WithPos {
  def meta: OptionTermMeta

  def sourcePos: Option[SourcePos] = meta.flatMap(_.sourcePos)
}

/** CallTerm has meta to trace runtime errors and debug */
sealed trait CallTerm extends TermWithMeta

sealed trait Term extends ToDoc {
  override def toDoc(implicit options: PrettierOptions): Doc = toString
}

case class ListTerm(terms: Vector[Term]) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist(Docs.`[`, Docs.`]`, ",")(terms *)
}

sealed trait Sort extends Term

case class Type(level: Term) extends Sort with Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist("Type" <> Docs.`(`, Docs.`)`)(level)
}

case object LevelType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("LevelType")
}

case class Level(n: Term) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Level" + n)
}

val Level0 = Level(IntegerTerm(0))

val Type0 = Type(Level0)

// Referencing Setω in Agda
case object Typeω extends Sort with Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Typeω").colored(ColorProfile.typeColor)
}

sealed trait LiteralTerm extends Term

case class IntegerTerm(value: BigInt) extends LiteralTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(value.toString).colored(ColorProfile.literalColor)
}

sealed trait TypeTerm extends Term

case object IntegerType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Integer").colored(ColorProfile.typeColor)
}

// int of 64 bits or more
case object IntType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Int").colored(ColorProfile.typeColor)
}

// unsigned int of 64 bits or more
case object UIntType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("UInt").colored(ColorProfile.typeColor)
}

case object NaturalType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Natural").colored(ColorProfile.typeColor)
}

case class RationalTerm(value: Rational) extends LiteralTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(value.toString).colored(ColorProfile.literalColor)
}

case class StringTerm(value: String) extends LiteralTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("\"" + encodeString(value) + "\"").colored(ColorProfile.literalColor)
}

case class SymbolTerm(value: String) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(":" + value).colored(ColorProfile.literalColor)
}

case object RationalType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Rational")
}

// float of 32 bits or more
case object FloatType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Float")
}

case object StringType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("String")
}

case object SymbolType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Symbol")
}

case object AnyType extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Any").colored(ColorProfile.typeColor)
}

case class ArgTerm(pattern: Term, ty: Term, default: Option[Term], vararg: Boolean) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = {
    val patternDoc = pattern.toDoc
    val tyDoc = ty.toDoc
    val defaultDoc = default.map(_.toDoc).getOrElse(Doc.empty)
    val varargDoc = if (vararg) Doc.text("...") else Doc.empty
    Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`:`)(patternDoc <+> tyDoc <+> defaultDoc <+> varargDoc)
  }
}

sealed trait TelescopeTerm extends Term

case class VisibleTelescopeTerm(implicitly: Boolean, args: Vector[ArgTerm]) extends TelescopeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Telescope")
}

case class InvisibleTelescopeTerm() extends TelescopeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("InvisibleTelescopeTerm")
}

case class ScopeId(id: VarId)

object ScopeId {
  def generate: ScopeId = ScopeId(VarId.generate)
}

case class Function(scope: ScopeId, ty: FunctionType, body: Term, meta: OptionTermMeta = None) extends TermWithMeta {
  override def toDoc(implicit options: PrettierOptions): Doc = {
    val tyDoc = ty.toDoc
    val bodyDoc = body.toDoc
    Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`->`)(tyDoc <+> bodyDoc)
  }
}

case class MatchingClause() {

}

case class Matching(scope: ScopeId, ty: FunctionType, clauses: Vector[MatchingClause], meta: OptionTermMeta = None) extends TermWithMeta {
  require(clauses.nonEmpty, "Matching should have at least one clause")

  override def toDoc(implicit options: PrettierOptions): Doc = toString // TODO
}

// Note that effect and result can use variables from telescope
case class FunctionType(restrictInScope: Vector[ScopeId], telescope: Vector[TelescopeTerm], effect: Term, resultTy: Term, meta: OptionTermMeta = None) extends TermWithMeta {
  override def toDoc(implicit options: PrettierOptions): Doc = {
    val telescopeDoc = telescope.map(_.toDoc).reduce(_ <+> _)
    val effectDoc = effect.toDoc
    val resultDoc = resultTy.toDoc
    Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`->`)(telescopeDoc <+> effectDoc <+> resultDoc)
  }
}

case class ObjectClauseValueTerm(key: Term, value: Term) {
  def toDoc(implicit options: PrettierOptions): Doc = group(key <+> Doc.text("=") <+> value)
}

case class ObjectTerm(clauses: Vector[ObjectClauseValueTerm]) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(clauses.map(_.toDoc): _*)
}


// exactFields is a hint: subtype relationship should not include different number of fields. Otherwise, throw a warning (only warning no error)
case class ObjectType(fieldTypes: Vector[ObjectClauseValueTerm], exactFields: Boolean = false) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist("Object" </> Docs.`{`, Docs.`}`, ",")(fieldTypes.map(_.toDoc): _*)
}

case class ListType(ty: Term) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist("List" <> Docs.`[`, Docs.`]`, ",")(ty)
}

case class OrType(xs: Vector[Term]) extends Term {
  require(xs.nonEmpty)

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist(Docs.`(`, Docs.`)`, " | ")(xs: _*)
}

case class AndType(xs: Vector[Term]) extends Term {
  require(xs.nonEmpty)

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist(Docs.`(`, Docs.`)`, " & ")(xs: _*)
}

sealed trait EffectTerm extends Term

case class EffectList(xs: Vector[Term]) extends EffectTerm {
  require(xs.nonEmpty)

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist(Docs.`[`, Docs.`]`, ",")(xs: _*)
}

object EffectList {
  // do flatten
  def apply(xs: Vector[Term]): EffectTerm = {
    val flattened = xs.flatMap {
      case EffectList(ys) => ys
      case x => Vector(x)
    }.filter {
      case NoEffect => false
      case _ => true
    }
    val distinct = flattened.distinct
    if (distinct.nonEmpty) new EffectList(distinct) else NoEffect
  }
}

case object NoEffect extends EffectTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("NoEffect")
}

// may raise an exception
case object ExceptionEffect extends EffectTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("PartialEffect")
}

// may not terminate
case object DivergeEffect extends EffectTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("DivergeEffect")
}

// whatever IO: console, file, network, ...
case object IOEffect extends EffectTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("IOEffect")
}

case object STEffect extends EffectTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("STEffect")
}

private object ResolvedVarCounter {
  var varIdCounter = 0
}

case class VarId(id: Int)

object VarId {
  def generate: VarId = ResolvedVarCounter.synchronized {
    ResolvedVarCounter.varIdCounter += 1
    VarId(ResolvedVarCounter.varIdCounter)
  }
}

sealed trait VarCall extends CallTerm {
  def varId: VarId
  def id: Id
}

case class LocalVarCall(id: Id, ty: Term, varId: VarId, meta: OptionTermMeta = None) extends VarCall {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(id)
}

case class ToplevelVarCall(module: QualifiedIDString, id: Id, ty: Term, varId: VarId, meta: OptionTermMeta = None) extends VarCall {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(module.mkString(".") + "." + id)
}

case class ErrorTerm(val error: TyckError) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(error.message)
}

case class MetaTerm(id: VarId, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("MetaTerm#" + id)
}

sealed trait StmtTerm {

}

case class NonlocalOrLocalReturn(scope: ScopeId, value: Term, meta: OptionTermMeta = None) extends StmtTerm {

}

case class BuiltinTerm(builtin: Builtin, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = builtin.toDoc
}


// TODO: tuple?
val UnitType = ObjectType(Vector.empty)