package chester.syntax

import chester.utils.{codePointIsEmoji, getCodePoints}
import chester.utils.parse.Character
import chester.utils.asInt
import java.lang.Character.{isDigit, isLetter}

object IdentifierRules {
  private val AllowedOperatorSymbols: Set[Int] = ".:=-+\\|<>/?`~!@$%^&*".toSet.map(_.asInt)
  private val AllowedWordingSymbols: Set[Int] = "_".toSet.map(_.asInt)
  private val AllowedMiddleWordingSymbols: Set[Int] = "-".toSet.map(_.asInt)
  val ReservedSymbols = ";,#()[]{}'\""

  private def isEmoji(codePoint: Int): Boolean = {
    codePointIsEmoji(codePoint)
  }

  private def isWording(x: Character): Boolean = isLetter(x) || isEmoji(x)

  def isOperatorSymbol(x: Character): Boolean = AllowedOperatorSymbols.contains(x)

  private def isWordingSymbol(x: Character): Boolean = AllowedWordingSymbols.contains(x)

  private def isMiddleWordingSymbol(x: Character): Boolean = {
    AllowedMiddleWordingSymbols.contains(x)
  }

  def isIdentifierFirst(x: Character): Boolean = isWording(x) || isWordingSymbol(x)

  def isIdentifierPart(x: Character): Boolean = {
    isIdentifierFirst(x) || isDigit(x) || isMiddleWordingSymbol(x)
  }

  def isIdentifierEnd(x: Character): Boolean = isIdentifierFirst(x) || isDigit(x)

  def isOperatorIdentifierFirst(x: Character): Boolean = isOperatorSymbol(x)

  def isOperatorIdentifierRest(x: Character): Boolean = {
    isOperatorSymbol(x) || isWordingSymbol(x)
  }

  def strIsOperator(s: String): Boolean = {
    val codepoints = s.getCodePoints
    if (codepoints.isEmpty) return false
    if (!isOperatorSymbol(codepoints.head)) return false
    codepoints.forall(isOperatorSymbol)
  }

}
