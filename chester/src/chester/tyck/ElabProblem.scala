package chester.tyck

import scala.language.experimental.genericNumberLiterals
import scala.collection.mutable

import chester.core.{AST, Arg, BuiltinEffect, CST, Coeffect, EffectRef, EnumCase, Implicitness, Param, StmtAST, Telescope}
import chester.error.{Problem, Reporter, Span, VectorReporter}
import chester.uniqid.{Uniqid, UniqidOf}
import chester.utils.elab.*
import chester.utils.{HoldNotReadable, given}
import chester.utils.doc.{<>, Doc, DocConf, DocOps, StringPrinter, ToDoc, given}
import chester.tyck.CoreTypeChecker.normalizeType
import cats.data.NonEmptyVector
/** Elaboration problems */
enum ElabProblem(val span0: Option[Span]) extends Problem:
  case UnboundVariable(name: String, override val span0: Option[Span]) extends ElabProblem(span0)
  case TypeMismatch(expected: AST, actual: AST, override val span0: Option[Span]) extends ElabProblem(span0)
  case NotAFunction(ty: AST, override val span0: Option[Span]) extends ElabProblem(span0)
  case NotAUniverse(ty: AST, override val span0: Option[Span]) extends ElabProblem(span0)
  case UnknownEffect(name: String, override val span0: Option[Span]) extends ElabProblem(span0)
  case UnitValueUsedAsType(override val span0: Option[Span]) extends ElabProblem(span0)
  case InternalElaborationFailure(message: String, override val span0: Option[Span]) extends ElabProblem(span0)

  override def stage: Problem.Stage = Problem.Stage.TYCK
  override def severity: Problem.Severity = Problem.Severity.Error

  override def toDoc(using DocConf): Doc = this match
    case ElabProblem.UnboundVariable(name, _) =>
      Doc.text(s"Unbound variable: $name")
    case ElabProblem.TypeMismatch(expected, actual, _) =>
      (Doc.text("Type mismatch: expected "): ToDoc) <> expected.toDoc <> Doc.text(", but got ") <> actual.toDoc
    case ElabProblem.NotAFunction(ty, _) =>
      (Doc.text("Not a function type: "): ToDoc) <> ty.toDoc
    case ElabProblem.NotAUniverse(ty, _) =>
      (Doc.text("Not a universe type: "): ToDoc) <> ty.toDoc
    case ElabProblem.UnknownEffect(name, _) =>
      Doc.text(s"Unknown effect: $name")
    case ElabProblem.UnitValueUsedAsType(_) =>
      Doc.text("Unit is the type; use Unit in type positions and () only as a value")
    case ElabProblem.InternalElaborationFailure(message, _) =>
      Doc.text(s"Internal elaboration failure: $message")

