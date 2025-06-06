package chester.elab

import chester.elab.ElabOps
import chester.syntax.core.*
import chester.elab.Context
import chester.utils.elab.*

import scala.language.implicitConversions

case object IsType extends Kind {
  type Of = IsType[Term, Nothing]
}

case class IsType[+T <: Term, -W](result: CellOf[T, W] | T)(using ctx: Context) extends Constraint(IsType) with ConstraintResult[CellOf[T, W] | T] {
  given Context = ctx
}

case object IsTypeHandler extends Handler[ElabOps, IsType.type](IsType) {
  implicit inline def writeTerm[T <: Term, W](inline x: CellOf[T, W] | T): CellRWOr[Term] = x.asInstanceOf[CellRWOr[Term]]
  override def run(c: IsType[Term, Nothing])(using ElabOps, SolverOps): Result = {
    import c.*
    toTerm(result) match {
      case _: MetaTerm[?] => Result.Waiting(assumeCell(result))
      case _              => Result.Done
    }
  }

  override def defaulting(constant: IsType[Term, Nothing], level: DefaultingLevel)(using ElabOps, SolverOps): Boolean = {
    import constant.*
    toTerm(result) match {
      case result: MetaTerm[?] =>
        assumeCell(result).fill(NothingType(meta = None))
        true
      case _ =>
        // do nothing
        false
    }
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = level == DefaultingLevel.IsType
}
