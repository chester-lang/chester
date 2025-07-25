package chester.error

import chester.i18n.*
import chester.syntax.Name
import chester.syntax.accociativity.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.utils.doc.*
import chester.utils.{union2RW, given}
import upickle.default.*

import scala.reflect.ClassTag

sealed trait TyckProblem extends Problem derives ReadWriter {
  final def stage: Problem.Stage = Problem.Stage.TYCK

  final def getMessage: String = {
    given options: DocConf = DocConf.Default
    render(toDoc)
  }

  def hint: ToDoc = empty

  override def toDoc(using options: DocConf): Doc

  // Only as a helper for defining span0
  def cause: Term | Expr

  override def span0: Option[Span] = cause match {
    case x: SpanOptional0 => x.span0
  }
}

sealed trait TyckError extends TyckProblem derives ReadWriter {
  override final def severity: Problem.Severity = Problem.Severity.Error
}

sealed trait TyckWarning extends TyckProblem derives ReadWriter {
  override final def severity: Problem.Severity = Problem.Severity.Warning
}

case class UnusedVariableWarning(id: Reference, cause: Expr) extends TyckWarning {
  override def toDoc(using
      DocConf
  ): Doc =
    d"Unused variable: $id"
}

given rwThis: ReadWriter[QualifiedName | String] =
  union2RW[Expr, String](using
    implicitly[ClassTag[Expr]],
    implicitly[ClassTag[String]],
    a = qualifiedNameRW.asInstanceOf[ReadWriter[Expr]],
    b = readwriter[String]
  ).asInstanceOf

case class ExpectCase(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"case clause must have a pattern and a return expression"
}

case class ExpectFullCaseBlock(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Expected a full case block, got "
}

case class ExpectLambda(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Expected a lambda expression, got "
}

case class ExpectLetDef(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Expected a let or def statement, got "
}

case class UnexpectedTelescope(cause: MaybeTelescope) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Unexpected telescope"
}

case class ExpectParameterList(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc =
    t"Expected a parameter list, got $cause"
}

case class UnsupportedTermError(cause: Term) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Unsupported term"
}

sealed trait OpInfoError extends TyckError derives ReadWriter {
  override def cause: Term | Expr = EmptyExpr
}

case class UnknownOperator(override val cause: Expr) extends OpInfoError {
  override def toDoc(using DocConf): Doc =
    t"Unknown operator."
}

case class PrecedenceCycleDetected(groups: Iterable[PrecedenceGroup]) extends OpInfoError {
  override def toDoc(using DocConf): Doc =
    t"Precedence cycle detected among groups: ${groups.map(_.name).mkString(" -> ")}"
}

case class UnexpectedTokens(tokens: List[Expr]) extends OpInfoError {
  override def toDoc(using DocConf): Doc =
    t"Unexpected tokens after parsing expression: $tokens"
}

case class UnknownPrecedenceGroup(group: PrecedenceGroup) extends OpInfoError {
  override def toDoc(using DocConf): Doc =
    t"Unknown precedence group: '${group.name}'."
}

case class UnconnectedPrecedenceGroups(
    group1: PrecedenceGroup,
    group2: PrecedenceGroup
) extends OpInfoError {
  override def toDoc(using DocConf): Doc =
    t"Precedence groups '${group1.name}' and '${group2.name}' are not connected."
}

case class UnboundVariable(name: Name, cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Unbound variable $name"
}

case class NotImplemented(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Not implemented ${cause.getClass.getName}"
}

case class TypeMismatch(lhs: Term, rhs: Term, cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc =
    Doc.text("Type mismatch: expected ") <> lhs.toDoc <> Doc.text(" but got ") <> rhs.toDoc <> cause.toDoc
}

case class DuplicateDefinition(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Duplicate definition"
}

case class FunctionCallUnificationError(
    functionType: Term,
    argumentTypes: Vector[Term],
    cause: Expr
) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc =
    t"Function call unification failed: expected $functionType but got $argumentTypes"

}

case class FunctionCallArityMismatchError(
    expected: Int,
    actual: Int,
    cause: Expr
) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Function expects $expected arguments, but got $actual"
}

case class FunctionCallArgumentMismatchError(
    expected: Int,
    actual: Int,
    cause: Expr
) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = t"Function expected $expected arguments, but received $actual"
}

case class ObjectFieldMismatch(
    missingInLHS: Seq[Term],
    missingInRHS: Seq[Term],
    cause: Expr
) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc = {
    val missingInLHSDoc = if (missingInLHS.nonEmpty) {
      Doc.text(t"Missing fields in LHS: ${missingInLHS.mkString(", ")}")
    } else Doc.empty
    val missingInRHSDoc = if (missingInRHS.nonEmpty) {
      Doc.text(t"Missing fields in RHS: ${missingInRHS.mkString(", ")}")
    } else Doc.empty
    missingInLHSDoc <> missingInRHSDoc
  }
}

case class InvalidImportSyntax(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc =
    t"Invalid syntax in import statement:"
}

case class InvalidModuleSyntax(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc =
    t"Invalid syntax in module statement:"
}

case class ExpectFieldDeclaration(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc =
    t"Expected a field declaration, got "
}

case class ExpectRecordName(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc =
    t"Expected a record name, got "
}
case class DuplicateFieldDefinition(cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Duplicate field definition in record"
}
case class UnsupportedExtendsType(cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    Doc.text("Unsupported type in extends clause")
}
case class NotATrait(cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Expected a trait, but got $cause"
}
case class NotImplementingTrait(recordName: Name, traitName: Name, cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Record '$recordName' does not implement trait '$traitName'"
}
case class RecordNotImplementingTrait(recordType: Term, traitType: Term, cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Type '$recordType' does not implement required trait '$traitType'"
}
case class MissingTraitField(fieldName: Name, recordName: Name, traitName: Name, cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Record '$recordName' is missing required field '$fieldName' from trait '$traitName'"
}
case class ExpectTraitName(cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Expected a trait name, got"
}

case class ExpectInterfaceName(cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Expected an interface name, got"
}
case class ExpectObjectName(cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Expected an object name, got"
}
case class MissingImplicitArgumentWarning(paramTy: Term, cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc =
    d"Implicit argument of type $paramTy is inferred for $cause"
}
case class PotentialNonterminatingFunction(cause: Expr) extends TyckError {
  override def toDoc(using
      DocConf
  ): Doc =
    t"Potential non-terminating function"
}

case class FieldNotFound(fieldName: Name, recordName: Name, cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Field '$fieldName' not found in record '$recordName'"
}

case class NotARecordType(ty: Term, cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Expected a record type, got $ty"
}

case class InvalidFieldName(cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Expected an identifier for field name"
}

case class NotImplementedFeature(message: String, cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"$message"
}

// Effect-related error types
case class CannotAddEffectError(effect: Term) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Cannot add effect ${effect.toDoc} to this context"

  override def cause: Term | Expr = effect
}

case class UnauthorizedEffectError(effect: Term, functionTerm: Term) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Function ${functionTerm.toDoc} uses effect ${effect.toDoc} but doesn't declare it"

  override def cause: Term | Expr = functionTerm
}

case class MissingEffectHandlerError(effect: Term) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Effect ${effect.toDoc} is used but not handled in the current scope"

  override def cause: Term | Expr = effect
}

// Elab-related error types for the elab module

// Used in BlockElab.scala where let.body is None
case class MissingLetBody(let: LetDefStmt) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Missing body expression in let statement"

  override def cause: Term | Expr = let
}

// Used in Unify.scala where the unification fails with no next constraint
case class UnificationError(lhs: Term, rhs: Term, cause: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Unification error: cannot unify ${lhs.toDoc} with ${rhs.toDoc}"
}

// Used in Lit.scala where a literal doesn't match the expected type
case class LiteralTypeMismatch(value: Term, expectedType: Term, expr: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Literal ${value.toDoc} cannot be used as type ${expectedType.toDoc}"

  override def cause: Term | Expr = expr
}

// Used in utils/elab/MergeSimple.scala
case class MergeError(lhs: Term, rhs: Term) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Cannot merge ${lhs.toDoc} with ${rhs.toDoc}"

  override def cause: Term | Expr = lhs
}

// General missing implementation error
case class MissingImplementation(context: String, expr: Expr) extends TyckError {
  override def toDoc(using DocConf): Doc =
    t"Missing implementation for $context"

  override def cause: Term | Expr = expr
}
