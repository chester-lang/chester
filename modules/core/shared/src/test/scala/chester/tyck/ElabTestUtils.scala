package chester.tyck

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.language.experimental.genericNumberLiterals

import chester.core.AST

/** Backwards-compatible test helpers that delegate to ElabRunner. */
object ElabTestUtils:
  given ExecutionContext = ExecutionContext.global
  val defaultTimeout: FiniteDuration = 10.seconds

  def runAsync(body: => Unit): Future[Unit] = Future(body)

  def elaborateExpr(input: String, ensureCoreType: Boolean = false): (Option[AST], Option[AST], Vector[ElabProblem]) =
    ElabRunner.elaborateExpr(input, ensureCoreType)

  def elaborateFile(input: String, ensureCoreType: Boolean = false): (Option[AST], Option[AST], Vector[ElabProblem]) =
    ElabRunner.elaborateFile(input, ensureCoreType)

  def elaborateModule(inputs: Seq[String], ensureCoreType: Boolean = false): (Seq[Option[AST]], Seq[Option[AST]], Vector[ElabProblem]) =
    ElabRunner.elaborateModule(inputs, ensureCoreType)
