// TODO: More correctly implement toDoc
package chester.syntax.core

import chester.doc.const.{ColorProfile, Docs}
import chester.doc.*
import chester.doc.Doc.group
import chester.error.*
import chester.syntax.{Id, QualifiedIDString}
import chester.utils.encodeString

import scala.language.implicitConversions

case class TermMeta(sourcePos: Option[SourcePos])

// so that it is not used in compare
case class OptionTermMeta(meta: Option[TermMeta] = None) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case _:OptionTermMeta => true
      case _ => false
    }
  }
}

implicit def toOptionTermMeta(meta: Option[TermMeta]): OptionTermMeta = new OptionTermMeta(meta)
implicit def fromOptionTermMeta(meta: OptionTermMeta): Option[TermMeta] = meta.meta

sealed trait Term extends WithPos with ToDoc {
  def meta: OptionTermMeta

  def sourcePos: Option[SourcePos] = meta.flatMap(_.sourcePos)
}

case class ListTerm(terms: Vector[Term], meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist(Docs.`[`, Docs.`]`, ",")(terms *)
}

sealed trait Sort extends Term

case class Type(level: Term, meta: OptionTermMeta = None) extends Sort {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist("Type"<>Docs.`(`, Docs.`)`)(level)
}

val Type0 = Type(IntegerTerm(0))

// Referencing Setω in Agda
case class Typeω(meta: OptionTermMeta = None) extends Sort {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Typeω").colored(ColorProfile.typeColor)
}

case class IntegerTerm(value: BigInt, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(value.toString).colored(ColorProfile.literalColor)
}

sealed trait TypeTerm extends Term

case class IntegerType(meta: OptionTermMeta = None) extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Integer").colored(ColorProfile.typeColor)
}

case class DoubleTerm(value: BigDecimal, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(value.toString).colored(ColorProfile.literalColor)
}

case class StringTerm(value: String, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("\"" + encodeString(value) + "\"").colored(ColorProfile.literalColor)
}

case class SymbolTerm(value: String, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(":" + value).colored(ColorProfile.literalColor)
}

case class DoubleType(meta: OptionTermMeta = None) extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Double")
}

case class StringType(meta: OptionTermMeta = None) extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("String")
}

case class SymbolType(meta: OptionTermMeta = None) extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Symbol")
}

case class AnyTerm(meta: OptionTermMeta = None) extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("Any").colored(ColorProfile.typeColor)
}

case class ObjectClauseValueTerm(key: Term, value: Term, meta: OptionTermMeta = None) {
  def toDoc(implicit options: PrettierOptions): Doc = group(key <+> Doc.text("=") <+> value)
}

case class ObjectTerm(clauses: Vector[ObjectClauseValueTerm], meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(clauses.map(_.toDoc): _*)
}


// exactFields is a hint: subtype relationship should not include different number of fields. Otherwise, throw a warning (only warning no error)
case class ObjectType(fieldTypes: Vector[ObjectClauseValueTerm], exactFields: Boolean = false, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist("Object"</> Docs.`{`, Docs.`}`, ",")(fieldTypes.map(_.toDoc): _*)
}

case class OrType(xs: Vector[Term], meta: OptionTermMeta = None) extends Term {
  require(xs.nonEmpty)

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist(Docs.`(`, Docs.`)`, " | ")(xs: _*)
}

case class AndType(xs: Vector[Term], meta: OptionTermMeta = None) extends Term {
  require(xs.nonEmpty)

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist(Docs.`(`, Docs.`)`, " & ")(xs: _*)
}

sealed trait EffectTerm extends Term

case class EffectList(xs: Vector[Term], meta: OptionTermMeta = None) extends EffectTerm {
  require(xs.nonEmpty)

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist(Docs.`[`, Docs.`]`, ",")(xs: _*)
}

object EffectList {
  // do flatten
  def apply(xs: Vector[Term], meta: OptionTermMeta = None): EffectTerm = {
    val flattened = xs.flatMap {
      case EffectList(ys, _) => ys
      case x => Vector(x)
    } .filter {
      case NoEffect(_) => false
      case _ => true
    }
    if(flattened.nonEmpty) new EffectList(flattened, meta) else NoEffect(meta)
  }
}

case class NoEffect(meta: OptionTermMeta = None) extends EffectTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("NoEffect")
}

case class PartialEffect(meta: OptionTermMeta = None) extends EffectTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("PartialEffect")
}

case class DivergeEffect(meta: OptionTermMeta = None) extends EffectTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("DivergeEffect")
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

case class LocalVar(id: Id, ty: Term, varId: VarId, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(id)
}

case class ResolvedIdenifier(module: QualifiedIDString, Id: Id, ty: Term, varId: VarId, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(module.mkString(".") + "." + Id)
}

case class ErrorTerm(val error: TyckError) extends Term {
  override def meta: OptionTermMeta = None

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(error.message)
}

case class MetaTerm(id: VarId, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("MetaTerm#" + id)
}


// TODO: tuple?
val UnitType = ObjectType(Vector.empty)