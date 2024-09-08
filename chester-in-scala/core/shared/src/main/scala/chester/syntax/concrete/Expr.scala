// TODO: Correctly implement toDoc. They are very broken
package chester.syntax.concrete

import chester.doc.*
import chester.error.*
import chester.syntax.concrete.stmt.QualifiedID
import chester.syntax.concrete.stmt.accociativity.Associativity
import chester.syntax.{Builtin, Id, QualifiedIDString, UnresolvedID}
import chester.utils.doc._
import chester.utils.{encodeString, reuse}
import spire.math.Rational
import upickle.default.*
import chester.utils.impls.*

enum CommentType derives ReadWriter {
  case OneLine
  case MultiLine
}

case class Comment(content: String, typ: CommentType, sourcePos: Option[SourcePos]) derives ReadWriter

case class CommentInfo(commentBefore: Vector[Comment], commentInBegin: Vector[Comment] = Vector.empty, commentInEnd: Vector[Comment] = Vector.empty, commentEndInThisLine: Vector[Comment] = Vector.empty) derives ReadWriter {
  if (commentBefore.isEmpty && commentInBegin.isEmpty && commentInEnd.isEmpty && commentEndInThisLine.isEmpty) {
    throw new IllegalArgumentException("At least one comment should be present")
  }
}

case class ExprMeta(sourcePos: Option[SourcePos], commentInfo: Option[CommentInfo]) derives ReadWriter {
  require(sourcePos.isDefined || commentInfo.isDefined)
}

object MetaFactory {
  def add(commentBefore: Vector[Comment] = Vector(), commentEndInThisLine: Vector[Comment] = Vector())(updateOn: Option[ExprMeta]): Option[ExprMeta] = (commentBefore, commentEndInThisLine, updateOn) match {
    case (Vector(), Vector(), _) => updateOn
    case (before, end, Some(ExprMeta(sourcePos, None))) => Some(ExprMeta(sourcePos, Some(CommentInfo(commentBefore = before, commentEndInThisLine = end))))
    case (before, end, Some(ExprMeta(sourcePos, Some(commentInfo)))) => Some(ExprMeta(sourcePos, Some(commentInfo.copy(commentBefore = before ++ commentInfo.commentBefore, commentEndInThisLine = commentInfo.commentEndInThisLine ++ end))))
  }
}

sealed trait Expr extends WithPos with ToDoc derives ReadWriter {
  def descent(operator: Expr => Expr): Expr = this

  final def descentRecursive(operator: Expr => Expr): Expr = {
    operator(this.descent(_.descentRecursive(operator)))
  }

  // Shouldn't use this.type
  protected final inline def thisOr(inline x: Expr): this.type = reuse(this, x.asInstanceOf[this.type])

  /** Every Expr has meta to trace compile time errors type checking errors */
  def meta: Option[ExprMeta]

  def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr

  def commentAtStart(comment: Comment): Expr = updateMeta {
    case Some(meta) => Some(meta.copy(commentInfo = meta.commentInfo.map(info => info.copy(commentBefore = info.commentBefore :+ comment))))
    case None => Some(ExprMeta(None, Some(CommentInfo(Vector(comment)))))
  }

  def commentAtStart(comment: Vector[Comment]): Expr = if (comment.isEmpty) this else updateMeta {
    case Some(meta) => Some(meta.copy(commentInfo = meta.commentInfo.map(info => info.copy(commentBefore = info.commentBefore ++ comment))))
    case None => Some(ExprMeta(None, Some(CommentInfo(comment))))
  }

  def sourcePos: Option[SourcePos] = meta.flatMap(_.sourcePos)

  def commentInfo: Option[CommentInfo] = meta.flatMap(_.commentInfo)

  override def toString: String = {
    implicit val options: PrettierOptions = PrettierOptions.Default
    render(this)
  }
}

sealed trait ParsedExpr extends Expr derives ReadWriter

sealed trait MaybeSaltedExpr extends Expr derives ReadWriter

case class Identifier(name: String, meta: Option[ExprMeta] = None) extends ParsedExpr derives ReadWriter {
  override def toString: String = meta.flatMap(_.sourcePos) match {
    case None => s"Identifier(\"${encodeString(name)}\")"
    case Some(pos) => s"Identifier(\"${encodeString(name)}\", ${pos.toString})"
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Identifier = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(name)

  def toSymbol: SymbolLiteral = SymbolLiteral(name, meta)
}

case class ResolvedIdentifier(module: QualifiedIDString, name: Id, meta: Option[ExprMeta] = None) extends Expr {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): ResolvedIdentifier = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(Doc.text(module.toString) <> Doc.text(".") <> Doc.text(name.toString))
}

case class OpSeq(seq: Vector[Expr], meta: Option[ExprMeta] = None) extends ParsedExpr with MaybeSaltedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    OpSeq(seq.map(operator), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): OpSeq = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(seq.map(_.toDoc).reduce(_ </> _))
}

// TODO: implement minimal parens
case class Infix(op: Expr, left: Expr, right: Expr, meta: Option[ExprMeta] = None) extends Expr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    Infix(operator(op), operator(left), operator(right), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Infix = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(left.toDoc </> op.toDoc </> right.toDoc)
}

case class Prefix(op: Expr, operand: Expr, meta: Option[ExprMeta] = None) extends Expr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    Prefix(operator(op), operator(operand), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Prefix = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(op.toDoc </> operand.toDoc)
}

case class Postfix(op: Expr, operand: Expr, meta: Option[ExprMeta] = None) extends Expr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    Postfix(operator(op), operator(operand), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Postfix = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(operand.toDoc </> op.toDoc)
}

case class Block(heads: Vector[Expr], tail: Option[Expr], meta: Option[ExprMeta] = None) extends ParsedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    Block(heads.map(operator), tail.map(operator), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Block = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group {
    val headDocs = heads.map(_.toDoc).reduceOption(_ <> Doc.text(";") </> _).getOrElse(Doc.empty)
    val tailDoc = tail.map(_.toDoc).getOrElse(Doc.empty)
    (Doc.text("{") </> Doc.indented(Doc.concat(headDocs, tailDoc)) </> Doc.text("}"))
  }
}

object Block {
  def apply(heads: Vector[Expr], tail: Expr): Block = Block(heads, Some(tail), None)

  def apply(heads: Vector[Expr], tail: Expr, meta: Option[ExprMeta]): Block = Block(heads, Some(tail), meta)
}

// maybe argument in function call or in function declaration
case class Arg(decorations: Vector[Identifier] = Vector(), name: Option[Identifier], ty: Option[Expr] = None, exprOrDefault: Option[Expr] = None, vararg: Boolean = false) derives ReadWriter {
  require(name.isDefined || exprOrDefault.isDefined)

  def descent(operator: Expr => Expr): Arg = {
    Arg(decorations, name, ty.map(operator), exprOrDefault.map(operator), vararg)
  }

  override def toString: String = this match {
    case Arg(Vector(), None, None, Some(expr), false) => s"Arg.of($expr)"
    case Arg(decorations, name, ty, exorOrDefault, false) => s"Arg($decorations,$name,$ty,$exorOrDefault)"
    case Arg(decorations, name, ty, exorOrDefault, vararg) => s"Arg($decorations,$name,$ty,$exorOrDefault,$vararg)"
  }

  def toDoc(implicit options: PrettierOptions): Doc = group {
    val decDoc = if (decorations.nonEmpty) decorations.map(_.toDoc).reduce(_ </> _) <> Doc.text(" ") else Doc.empty
    val nameDoc = name.map(_.toDoc).getOrElse(Doc.empty)
    val tyDoc = ty.map(t => Doc.text(": ") <> t.toDoc).getOrElse(Doc.empty)
    val exprDoc = exprOrDefault.map(e => Doc.text(" = ") <> e.toDoc).getOrElse(Doc.empty)
    val varargDoc = if (vararg) Doc.text("...") else Doc.empty
    decDoc <> nameDoc <> tyDoc <> exprDoc <> varargDoc
  }
}

object Arg {
  def of(expr: Expr): Arg = Arg(Vector.empty, None, None, Some(expr))
}

sealed trait MaybeTelescope extends Expr derives ReadWriter {
  override def descent(operator: Expr => Expr): MaybeTelescope = thisOr(super.descent(operator).asInstanceOf[MaybeTelescope])
}

sealed trait ParsedMaybeTelescope extends MaybeTelescope with ParsedExpr derives ReadWriter

case class Tuple(terms: Vector[Expr], meta: Option[ExprMeta] = None) extends ParsedMaybeTelescope {
  override def descent(operator: Expr => Expr): Tuple = thisOr {
    Tuple(terms.map(operator), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Tuple = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist("(", ")", ", ")(terms: _*)
}

case class Telescope(args: Vector[Arg], implicitly: Boolean = false, meta: Option[ExprMeta] = None) extends MaybeTelescope with DesaltExpr {
  override def descent(operator: Expr => Expr): Telescope = thisOr {
    Telescope(args.map(_.descent(operator)), implicitly, meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Telescope = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist("(", ")", ", ")(args.map(_.toDoc) *)
}

object Telescope {
  def of(args: Arg*)(implicit meta: Option[ExprMeta] = None): Telescope = Telescope(args.toVector, meta = meta)
}

case class FunctionCall(function: Expr, telescope: MaybeTelescope, meta: Option[ExprMeta] = None) extends ParsedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    FunctionCall(operator(function), telescope.descent(operator), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): FunctionCall = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(function.toDoc </> telescope.toDoc)
}

object FunctionCall {
  def calls(function: Expr, telescopes: Seq[MaybeTelescope]): FunctionCall = {
    if (telescopes.isEmpty) throw new IllegalArgumentException("At least one telescope should be present")
    if (telescopes.tail.isEmpty) return FunctionCall(function, telescopes.head)
    val head = FunctionCall(function, telescopes.head)
    calls(head, telescopes.tail)
  }
}

case class DotCall(expr: Expr, field: Expr, telescope: Vector[MaybeTelescope], meta: Option[ExprMeta] = None) extends ParsedExpr derives ReadWriter {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    DotCall(operator(expr), operator(field), telescope.map(_.descent(operator)), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): DotCall = copy(meta = updater(meta))

  def isField: Boolean = telescope.isEmpty

  def isQualifiedName: Boolean = {
    if (telescope.nonEmpty) return false
    if (!field.isInstanceOf[Identifier]) return false
    expr.match {
      case Identifier(_, _) => true
      case DotCall(_, _, _, _) => expr.asInstanceOf[DotCall].isQualifiedName
      case _ => false
    }
  }

  override def toDoc(implicit options: PrettierOptions): Doc = group(expr.toDoc <> Doc.text(".") <> field.toDoc <> telescope.map(_.toDoc).reduceOption(_ <> _).getOrElse(Doc.empty))
}

type QualifiedName = DotCall | Identifier
implicit val qualifiedNameRW: ReadWriter[QualifiedName] = union2RW[DotCall, Identifier]

object QualifiedName {
  def build(x: QualifiedName, field: Identifier, meta: Option[ExprMeta] = None): QualifiedName = DotCall(x, field, Vector(), meta)
}

sealed trait Literal extends ParsedExpr derives ReadWriter

case class IntegerLiteral(value: BigInt, meta: Option[ExprMeta] = None) extends Literal {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(value.toString)
}
case class RationalLiteral(value: Rational, meta: Option[ExprMeta] = None) extends Literal {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(value.toString)
}

case class StringLiteral(value: String, meta: Option[ExprMeta] = None) extends Literal {
  override def toString: String = meta.flatMap(_.sourcePos) match {
    case None => s"StringLiteral(\"${encodeString(value)}\")"
    case Some(pos) => s"StringLiteral(\"${encodeString(value)}\", ${pos.toString})"
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("\"" + encodeString(value) + "\"")
}

case class SymbolLiteral(value: String, meta: Option[ExprMeta] = None) extends Literal {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(":" + value)

  def toIdentifier: Identifier = Identifier(value, meta)
}

case class ListExpr(terms: Vector[Expr], meta: Option[ExprMeta] = None) extends ParsedMaybeTelescope {
  override def descent(operator: Expr => Expr): ListExpr = thisOr {
    ListExpr(terms.map(operator), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): ListExpr = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist("[", "]", ", ")(terms: _*)
}

case class HoleExpr(description: String, meta: Option[ExprMeta] = None) extends Expr {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): HoleExpr = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(Doc.text("?") <> Doc.text(description))
}

case class TypeAnnotation(expr: Expr, ty: Expr, meta: Option[ExprMeta] = None) extends DesaltExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    TypeAnnotation(operator(expr), operator(ty), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): TypeAnnotation = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(expr.toDoc <> Doc.text(": ") <> ty.toDoc)
}

case class AnnotatedExpr(annotation: Identifier, telescope: Vector[MaybeTelescope], expr: Expr, meta: Option[ExprMeta] = None) extends ParsedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    AnnotatedExpr(annotation, telescope.map(_.descent(operator)), operator(expr), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): AnnotatedExpr = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(annotation.toDoc <> telescope.map(_.toDoc).reduceOption(_ <> _).getOrElse(Doc.empty) <> expr.toDoc)
}

sealed trait ObjectClause derives ReadWriter {
  def descent(operator: Expr => Expr): ObjectClause = this match {
    case ObjectExprClause(k, v) => ObjectExprClause(k, operator(v))
    case ObjectExprClauseOnValue(k, v) => ObjectExprClauseOnValue(operator(k), operator(v))
  }
}

case class ObjectExprClause(key: QualifiedName, value: Expr) extends ObjectClause {
}

case class ObjectExprClauseOnValue(key: Expr, value: Expr) extends ObjectClause {
}

object ObjectExprClause {
  def apply(key: QualifiedName, value: Expr): ObjectExprClause = new ObjectExprClause(key, value)
}

@deprecated
implicit def toObjectExprClause(pair: (QualifiedName, Expr)): ObjectExprClause = ObjectExprClause(pair._1, pair._2)

case class ObjectExpr(clauses: Vector[ObjectClause], meta: Option[ExprMeta] = None) extends ParsedExpr with MaybeSaltedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    ObjectExpr(clauses.map(_.descent(operator)), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): ObjectExpr = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist("{", "}", ", ")(
    clauses.map {
      case ObjectExprClause(key, value) => key.toDoc <> Doc.text("=") <> value.toDoc
      case ObjectExprClauseOnValue(key, value) => key.toDoc <> Doc.text("=>") <> value.toDoc
    } *
  )
}

case class Keyword(key: Id, telescope: Vector[MaybeTelescope], meta: Option[ExprMeta] = None) extends ParsedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    Keyword(key, telescope.map(_.descent(operator)), meta)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Keyword = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(Doc.text("#" + key) <> telescope.map(_.toDoc).reduceOption(_ <> _).getOrElse(Doc.empty))
}

sealed trait DesaltExpr extends Expr derives ReadWriter

case class DesaltCaseClause(pattern: Expr, returning: Expr, meta: Option[ExprMeta] = None) extends DesaltExpr {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): DesaltCaseClause = copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): DesaltCaseClause = thisOr(DesaltCaseClause(operator(pattern), operator(returning), meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(Doc.text("case ") <> pattern.toDoc <> Doc.text(" => ") <> returning.toDoc)
}

case class DesaltMatching(clauses: Vector[DesaltCaseClause], meta: Option[ExprMeta] = None) extends DesaltExpr {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): DesaltMatching = copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): Expr = thisOr(DesaltMatching(clauses.map(_.descent(operator)), meta))

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.wrapperlist("{", "}", ";")(
    clauses.map(_.toDoc) *
  )
}

case class FunctionExpr(telescope: Vector[Telescope], effect: Option[Expr] = None, result: Option[Expr] = None, body: Expr, meta: Option[ExprMeta] = None) extends DesaltExpr {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): FunctionExpr = copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): Expr = thisOr(FunctionExpr(telescope.map(_.descent(operator)), effect.map(operator), result.map(operator), operator(body), meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group {
    val telescopeDoc = telescope.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    val effectDoc = effect.map(e => Doc.text(" ") <> e.toDoc).getOrElse(Doc.empty)
    val resultDoc = result.map(r => Doc.text(" -> ") <> r.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.toDoc
    telescopeDoc <> effectDoc <> resultDoc <> Doc.text(" {") </> Doc.indented(bodyDoc) </> Doc.text("}")
  }
}


sealed trait ErrorExpr extends Expr derives ReadWriter

case object EmptyExpr extends ErrorExpr {
  override def meta: Option[ExprMeta] = None

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): EmptyExpr.type = throw new UnsupportedOperationException()

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("EmptyExpr")
}

case class DesaltFailed(origin: Expr, error: TyckErrorOrWarning, meta: Option[ExprMeta] = None) extends ErrorExpr {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): DesaltFailed = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = group(Doc.text("DesaltFailed(") <> origin.toDoc <> Doc.text(", ") <> error.toDoc <> Doc.text(")"))
}

sealed trait DesaltPattern extends DesaltExpr derives ReadWriter {
  override def descent(operator: Expr => Expr): DesaltPattern = this
  def bindings: Vector[Identifier] = Vector.empty
}

case class PatternCompare(literal: Expr, meta: Option[ExprMeta]) extends DesaltPattern {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): PatternCompare = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = literal.toDoc
}

case class PatternBind(name: Identifier, meta: Option[ExprMeta]) extends DesaltPattern {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): PatternBind = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = name.toDoc

  override def bindings: Vector[Identifier] = Vector(name)
}

case class Bindings(forwardingBindings: Vector[Identifier]=Vector.empty, sequentialBindings: Vector[Identifier]=Vector.empty) {
}

object Bindings {
  def reduce(bindings: Seq[Bindings]): Bindings = {
    val forwardingBindings = bindings.toVector.flatMap(_.forwardingBindings)
    val sequentialBindings = bindings.toVector.flatMap(_.sequentialBindings)
    Bindings(forwardingBindings, sequentialBindings)
  }
}

sealed trait Stmt extends DesaltExpr derives ReadWriter  {
  def bindings: Bindings = Bindings(Vector.empty, Vector.empty)

  def reduceOnce: (Vector[TyckWarning], Vector[TyckError], Stmt) = (Vector.empty, Vector.empty, this)

  def reduce: (Vector[TyckWarning], Vector[TyckError], Stmt) = {
    val (warnings, errors, stmt) = reduceOnce
    if (stmt == this) (Vector.empty, Vector.empty, stmt)
    else {
      val (newWarnings, newErrors, newStmt) = stmt.reduce
      (warnings ++ newWarnings, errors ++ newErrors, newStmt)
    }
  }
}

@deprecated("not used")
private sealed trait NameUnknown extends Stmt derives ReadWriter  {
  def getName: Option[Id] = None
}

@deprecated("not used")
private sealed trait NameOption extends Stmt derives ReadWriter  {
  def name: Option[Id]

  def getName: Option[Id] = name
}

@deprecated("not used")
case class DataStmt(rest: Vector[Expr], meta: Option[ExprMeta] = None) extends Stmt with NameUnknown {
  override def toDoc(implicit options: PrettierOptions): Doc = group(Doc.text("data") <+> rest.map(_.toDoc).reduce(_ <+> _))

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

@deprecated("not used")
case class TraitStmt(rest: Vector[Expr], meta: Option[ExprMeta] = None) extends Stmt with NameUnknown {
  override def toDoc(implicit options: PrettierOptions): Doc = group(Doc.text("trait") <+> rest.map(_.toDoc).reduce(_ <+> _))

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

@deprecated("not used")
case class ImplementStmt(rest: Vector[Expr], meta: Option[ExprMeta] = None) extends Stmt with NameUnknown {
  override def toDoc(implicit options: PrettierOptions): Doc = group(Doc.text("implement") <+> rest.map(_.toDoc).reduce(_ <+> _))

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

@deprecated("not used")
case class TypeDeclaration(name: Option[String], exprs: Vector[Expr], types: Vector[Expr], meta: Option[ExprMeta] = None) extends NameOption {
  def toDoc(implicit options: PrettierOptions): Doc = group {
    val nameDoc = name.map(Doc.text(_)).getOrElse(Doc.empty)
    val exprsDoc = exprs.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    val typesDoc = types.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    nameDoc <+> exprsDoc <+> Doc.text(": ") <+> typesDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

@deprecated("not used")
case class Definition(name: Option[String], exprs: Vector[Expr], defns: Vector[Expr], meta: Option[ExprMeta] = None) extends NameOption {
  def toDoc(implicit options: PrettierOptions): Doc = group {
    val nameDoc = name.map(Doc.text(_)).getOrElse(Doc.empty)
    val exprsDoc = exprs.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    val defnsDoc = defns.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    nameDoc <+> exprsDoc <+> Doc.text(" = ") <+> defnsDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

@deprecated("not used")
case class DeclarationAndDefinition(name: Option[String], declNameExprs: Vector[Expr], types: Vector[Expr], defns: Vector[Expr], meta: Option[ExprMeta] = None) extends NameOption {
  def toDoc(implicit options: PrettierOptions): Doc = group {
    val nameDoc = name.map(Doc.text(_)).getOrElse(Doc.empty)
    val declNameExprsDoc = declNameExprs.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    val typesDoc = types.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    val defnsDoc = defns.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    nameDoc <+> declNameExprsDoc <+> Doc.text(": ") <+> typesDoc <+> Doc.text(" = ") <+> defnsDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

case class ExprStmt(expr: Expr, meta: Option[ExprMeta] = None) extends Stmt {
  override def toDoc(implicit options: PrettierOptions): Doc = expr.toDoc

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

@deprecated("not used")
case class GroupedStmt(name: Option[Id], declaration: TypeDeclaration, definitions: Vector[Definition], meta: Option[ExprMeta] = None) extends NameOption {
  def toDoc(implicit options: PrettierOptions): Doc = group {
    val nameDoc = name.map(Doc.text(_)).getOrElse(Doc.empty)
    val declDoc = declaration.toDoc
    val defnsDoc = definitions.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    nameDoc <+> declDoc <+> defnsDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

@deprecated("not used")
case class ErrorStmt(name: Option[Id], message: String, meta: Option[ExprMeta] = None) extends NameOption {
  def toDoc(implicit options: PrettierOptions): Doc = group {
    val nameDoc = name.map(Doc.text(_)).getOrElse(Doc.empty)
    nameDoc <+> Doc.text("error: ") <+> Doc.text(message)
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

@deprecated("not used")
sealed trait PrecedenceGroup

@deprecated("not used")
case class PrecedenceGroupResolving(
                                     name: Id,
                                     higherThan: Vector[UnresolvedID] = Vector(),
                                     lowerThan: Vector[UnresolvedID] = Vector(),
                                     associativity: Associativity = Associativity.None, meta: Option[ExprMeta] = None
                                   ) extends Stmt with PrecedenceGroup {
  def getName: Option[Id] = Some(name)

  override def toDoc(implicit options: PrettierOptions): Doc = group {
    val nameDoc = name.toDoc
    val higherThanDoc = if (higherThan.isEmpty) Doc.empty else Doc.text("higher than ") <> higherThan.map(_.toString).mkString
    val lowerThanDoc = if (lowerThan.isEmpty) Doc.empty else Doc.text("lower than ") <> lowerThan.map(_.toString).mkString
    val associativityDoc = associativity match {
      case Associativity.None => Doc.empty
      case Associativity.Left => Doc.text("associativity left")
      case Associativity.Right => Doc.text("associativity right")
    }
    nameDoc <+> higherThanDoc <+> lowerThanDoc <+> associativityDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

@deprecated("not used")
case class PrecedenceGroupResolved(
                                    name: QualifiedID,
                                    higherThan: Vector[PrecedenceGroupResolved] = Vector(),
                                    lowerThan: Vector[PrecedenceGroupResolved] = Vector(),
                                    associativity: Associativity = Associativity.None, meta: Option[ExprMeta] = None
                                  ) extends Stmt with PrecedenceGroup {
   def getName: Option[Id] = Some(name.name)

  override def toDoc(implicit options: PrettierOptions): Doc = group {
    val nameDoc = name.toString
    val higherThanDoc = if (higherThan.isEmpty) Doc.empty else Doc.text("higher than ") <> higherThan.map(_.toDoc).reduce(_ <+> _)
    val lowerThanDoc = if (lowerThan.isEmpty) Doc.empty else Doc.text("lower than ") <> lowerThan.map(_.toDoc).reduce(_ <+> _)
    val associativityDoc = associativity match {
      case Associativity.None => Doc.empty
      case Associativity.Left => Doc.text("associativity left")
      case Associativity.Right => Doc.text("associativity right")
    }
    nameDoc <+> higherThanDoc <+> lowerThanDoc <+> associativityDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

enum LetDefType derives ReadWriter {
  case Let
  case Def
}

sealed trait Defined extends ToDoc derives ReadWriter {
  def bindings: Vector[Identifier]
}

case class DefinedPattern(pattern: DesaltPattern) extends Defined {
  override def toDoc(implicit options: PrettierOptions): Doc = pattern.toDoc
  def bindings: Vector[Identifier] = pattern.bindings
}
case class DefinedFunction(id: Identifier, telescope: Vector[MaybeTelescope]) extends Defined{
  require(telescope.nonEmpty)
  def bindings: Vector[Identifier] = Vector(id)
  override def toDoc(implicit options: PrettierOptions): Doc = group(id.toDoc <> telescope.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty))
}

case class LetDefStmt(kind: LetDefType, defined: Defined, body: Option[Expr] = None, ty: Option[Expr] = None, decorations: Vector[Expr] = Vector(), meta: Option[ExprMeta] = None) extends Stmt {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    LetDefStmt(kind, defined, body.map(operator), ty.map(operator), decorations.map(operator), meta)
  }

  override def bindings: Bindings = kind match {
    case LetDefType.Let => Bindings(sequentialBindings=defined.bindings)
    case LetDefType.Def => Bindings(forwardingBindings=defined.bindings)
  }


  override def toDoc(implicit options: PrettierOptions): Doc = group {
    val kindDoc = kind match {
      case LetDefType.Let => Doc.text("let")
      case LetDefType.Def => Doc.text("def")
    }
    val definedDoc = defined.toDoc
    val tyDoc = ty.map(t => Doc.text(": ") <> t.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(b => Doc.text(" = ") <> b.toDoc).getOrElse(Doc.empty)
    val decorationsDoc = if (decorations.isEmpty) Doc.empty else decorations.map(_.toDoc).reduce(_ <+> _)
    decorationsDoc <+> kindDoc <+> definedDoc <+> tyDoc <+> bodyDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

case class ReturnStmt(expr: Expr, meta: Option[ExprMeta] = None) extends Stmt {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    ReturnStmt(operator(expr), meta)
  }

  override def toDoc(implicit options: PrettierOptions): Doc = group(Doc.text("return ") <> expr.toDoc)

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr = copy(meta = updater(meta))
}

case class BuiltinExpr(builtin: Builtin, meta: Option[ExprMeta] = None) extends Expr {
  override def descent(operator: Expr => Expr): Expr = this
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): BuiltinExpr = copy(meta = updater(meta))

  override def toDoc(implicit options: PrettierOptions): Doc = builtin.toDoc
}