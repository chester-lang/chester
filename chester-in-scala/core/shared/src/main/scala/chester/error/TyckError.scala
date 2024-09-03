package chester.error

import chester.doc.{PrettierOptions, *}
import chester.i18n.*
import chester.syntax.concrete.*
import chester.syntax.core.Term

sealed abstract class TyckErrorOrWarning extends Exception with ToDoc {
  def message: String = render(toDoc)

  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = message

  def cause: Term | Expr

  def location: Option[SourcePos] = cause match {
    case x: WithPos => x.sourcePos
    case _ => None
  }

  val stack: Array[StackTraceElement] = this.getStackTrace

  def renderWithLocation(implicit options: PrettierOptions = PrettierOptions.Default): Doc = {
    val baseMessage = Doc.text(t"Error: ") <> Doc.text(message).colored(Attribute.BoldOn)

    val locationInfo = location match {
      case Some(pos) =>
        val lines = pos.getLinesInRange.map { case (lineNumber, line) =>
          Doc.text(t"$lineNumber: ") <> Doc.text(line).colored(Attribute.BoldOn)
        }
        val locationHeader = Doc.text(t"Location: ") <>
          Doc.text(t"${pos.fileName} [${pos.range.start.line + 1}:${pos.range.start.column + 1}] to [${pos.range.end.line + 1}:${pos.range.end.column + 1}]").colored(Attribute.BoldOn)

        val codeBlock = Doc.group(Doc.concat(lines.map(_ <|> Doc.empty): _*))

        locationHeader <|> codeBlock

      case None =>
        val causeHeader = Doc.text(t"Cause: ").colored(Attribute.BoldOn)
        val causeText = cause.toDoc
        causeHeader <|> causeText
    }

    baseMessage <|> locationInfo
  }
}

abstract class TyckError extends TyckErrorOrWarning {
}

abstract class TyckWarning extends TyckErrorOrWarning {

}

case class EmptyResultsError() extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Empty Results"

  override def cause: Expr = EmptyExpr
}

case class UnifyFailedError(rhs: Term, lhs: Term) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Unification failed: $rhs is not a subtype of $lhs"

  override def cause: Term = rhs
}

case class UnsupportedExpressionError(expr: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Unsupported expression type: $expr"

  override def cause: Expr = expr
}

case class IdentifierNotFoundError(cause: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Identifier not found: $cause"
}

case class UnexpectedStmt(x: Stmt) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc =  t"Unexpected statement: $x"

  override def cause: Expr = x
}

case class FieldTypeNotFoundError(qualifiedName: QualifiedName | String) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Field type not found for $qualifiedName"

  override def cause: Term | Expr = qualifiedName match {
    case x: Term => x
    case x: Expr => x
    case x: String => EmptyExpr
  }
}

case class ExpectedObjectTypeError() extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Expected an ObjectType for inheritance"

  override def cause: Term | Expr = EmptyExpr
}


case class ExpectCase(cause: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"case clause must have a pattern and a return expression"
}

case class ExpectFullCaseBlock(block: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Expected a full case block, got $block"

  override def cause: Expr = block
}


case class ExpectSingleExpr(xs: Seq[Expr]) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Expected a single expression, got $xs"

  override def cause: Expr = xs.head
}

case class ExpectLambda(x: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Expected a lambda expression, got $x"

  override def cause: Expr = x
}

case class ExpectPattern(cause: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc = t"Expected a pattern, got $cause"
}