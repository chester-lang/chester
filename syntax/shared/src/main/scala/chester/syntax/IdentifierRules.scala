package chester.syntax

import chester.utils.{codePointIsEmoji, getCodePoints}
import chester.utils.parse.Character

import java.lang.Character.{isDigit, isLetter}

object IdentifierRules {
  val AllowedOperatorSymbols: Set[Int] = ".:=-+\\|<>/?`~!@$%^&*".toSet.map(_.toInt)
  val AllowedWordingSymbols: Set[Int] = "_".toSet.map(_.toInt)
  val AllowedMiddleWordingSymbols: Set[Int] = "-".toSet.map(_.toInt)
  val ReservedSymbols = ";,#()[]{}'\""

  def isEmoji(codePoint: Int): Boolean =
    codePointIsEmoji(codePoint)

  def isWording(x: Character): Boolean = isLetter(x) || isEmoji(x)

  def isOperatorSymbol(x: Character): Boolean = AllowedOperatorSymbols.contains(x)

  def isWordingSymbol(x: Character): Boolean = AllowedWordingSymbols.contains(x)

  def isMiddleWordingSymbol(x: Character): Boolean =
    AllowedMiddleWordingSymbols.contains(x)

  def isIdentifierFirst(x: Character): Boolean = isWording(x) || isWordingSymbol(x)

  def isIdentifierPart(x: Character): Boolean =
    isIdentifierFirst(x) || isDigit(x) || isMiddleWordingSymbol(x)

  def isIdentifierEnd(x: Character): Boolean = isIdentifierFirst(x) || isDigit(x)

  def isOperatorIdentifierFirst(x: Character): Boolean = isOperatorSymbol(x)

  def isOperatorIdentifierRest(x: Character): Boolean =
    isOperatorSymbol(x) || isWordingSymbol(x)

  def strIsOperator(s: String): Boolean = {
    val codepoints = s.getCodePoints
    if (codepoints.isEmpty) return false
    if (!isOperatorSymbol(codepoints.head)) return false
    codepoints.forall(isOperatorSymbol)
  }

}
