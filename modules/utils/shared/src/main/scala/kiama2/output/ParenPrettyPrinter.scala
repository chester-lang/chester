/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2021 Anthony M Sloane, Macquarie University.
 * 2024 Mio. obtained from https://github.com/inkytonik/kiama/tree/3bcc03ae08e60c79b3c4b9bad3ae9ae233979d4a. modified
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama2.output

import kiama2.output.PrettyPrinterTypes.Width

/** The sides that an expression may appear on inside another expression or associativities that infix operators can have.
  */
abstract class Side

/** The left side or left associativitiy of an infix operator.
  */
case object LeftAssoc extends Side

/** The right side or right associativitiy of an infix operator.
  */
case object RightAssoc extends Side

/** No side or non-associativitiy of an infix operator.
  */
case object NonAssoc extends Side

/** The possible fixities of operators.
  */
abstract class Fixity

/** The unary operator occurs in prefix position (i.e., before its operand).
  */
case object Prefix extends Fixity

/** The unary operator occurs in postfix position (i.e., after its operand).
  */
case object Postfix extends Fixity

/** The binary operator occurs in infix position (i.e., between its two operands).
  */
case class Infix(side: Side) extends Fixity

/** Super type of all expressions that are to be pretty-printed.
  */
sealed trait PrettyExpression[Doc]

implicit class PrettyExpressionDoc[Doc](val doc: Doc) extends PrettyExpression[Doc]

/** An expression that contains an operator. Defines `priority` to relate the operator to other operators (lower number is higher priority, no
  * default). Also defines `fixity` to specify the relationship between the operator and its operand(s) (no default).
  */
sealed trait PrettyOperatorExpression[Doc] extends PrettyExpression[Doc] {
  def priority: Int

  def fixity: Fixity
}

/** Binary expressions that are to be pretty-printed. `left` and `right` give the two operand expressions and `op` the string that is to be used as
  * the output of the operator.
  */
trait PrettyBinaryExpression[Doc] extends PrettyOperatorExpression[Doc] {
  def left: PrettyExpression[Doc]

  def op: Doc

  def right: PrettyExpression[Doc]
}

/** Unary expressions that are to be pretty-printed. `exp` gives the operand expressions and `op` the string that is to be used as the output of the
  * operator.
  */
trait PrettyUnaryExpression[Doc] extends PrettyOperatorExpression[Doc] {
  def op: Doc

  def exp: PrettyExpression[Doc]
}

/** A pretty-printer with support for pretty-printing expressions with minimal parenthesisation.
  *
  * Based on algorithm in "Unparsing expressions with prefix and postfix operators", Ramsey, SP&E, 28 (12), October 1998. We have not implemented
  * support for arbitrary arity infix operators.
  */
trait ParenPrettyPrinter extends AbstractPrettyPrinter {

  type Expr = PrettyExpression[Doc]

  /** Pretty-print a recursive child reference, parenthesizing if necessary.
    */
  def recursiveToDoc(
      outer: PrettyOperatorExpression[Doc],
      inner: PrettyExpression[Doc],
      side: Side
  ): Doc =
    inner match {
      case l: PrettyOperatorExpression[Doc] =>
        bracket(outer, l, side)
      case l =>
        toParenDoc(l)
    }

  /** Pretty-print a unary, binary or nary expression.
    */
  def toParenDoc(e: PrettyExpression[Doc]): Doc =
    e match {

      case b: PrettyBinaryExpression[Doc] =>
        val ld = recursiveToDoc(b, b.left, LeftAssoc)
        val rd = recursiveToDoc(b, b.right, RightAssoc)
        ld <+> b.op <+> rd

      case u: PrettyUnaryExpression[Doc] =>
        val ed = recursiveToDoc(u, u.exp, NonAssoc)
        if (u.fixity == Prefix)
          u.op <> ed
        else
          ed <> u.op

      case d: PrettyExpressionDoc[Doc] =>
        d.doc
    }

  /** Optionally parenthesise an operator expression based on the precedence relation with an outer expression's operator.
    */
  def bracket(
      outer: PrettyOperatorExpression[Doc],
      inner: PrettyOperatorExpression[Doc],
      side: Side
  ): Doc = {
    val d = toParenDoc(inner)
    if (noparens(outer, inner, side)) d else parens(d)
  }

  /** Return true if the inner expression should not be parenthesised when appearing on the given side with the outer expression.
    */
  def noparens(
      outer: PrettyOperatorExpression[Doc],
      inner: PrettyOperatorExpression[Doc],
      side: Side
  ): Boolean = {
    val pi = inner.priority
    val po = outer.priority
    lazy val fi = inner.fixity
    lazy val fo = outer.fixity
    (pi < po) ||
    ((fi, side) match {
      case (Postfix, LeftAssoc) =>
        true
      case (Prefix, RightAssoc) =>
        true
      case (Infix(LeftAssoc), LeftAssoc) =>
        (pi == po) && (fo == Infix(LeftAssoc))
      case (Infix(RightAssoc), RightAssoc) =>
        (pi == po) && (fo == Infix(RightAssoc))
      case (_, NonAssoc) =>
        fi == fo
      case _ =>
        false
    })
  }

  def prettyExpr(d: Expr, w: Width = defaultWidth): Document =
    pretty(toParenDoc(d), w)

}
