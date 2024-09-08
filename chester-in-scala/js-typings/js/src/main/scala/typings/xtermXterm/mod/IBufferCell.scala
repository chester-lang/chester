package typings.xtermXterm.mod

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/**
  * Represents a single cell in the terminal's buffer.
  */
trait IBufferCell extends StObject {
  
  /**
    * Gets a cell's background color number, this differs depending on what the
    * color mode of the cell is:
    *
    * - Default: This should be 0, representing the default background color
    *   (CSI 49 m).
    * - Palette: This is a number from 0 to 255 of ANSI colors
    *   (CSI 4(0-7) m, CSI 10(0-7) m, CSI 48 ; 5 ; 0-255 m).
    * - RGB: A hex value representing a 'true color': 0xRRGGBB
    *   (CSI 4 8 ; 2 ; Pi ; Pr ; Pg ; Pb)
    */
  def getBgColor(): Double
  
  /**
    * Gets the number representation of the background color mode, this can be
    * used to perform quick comparisons of 2 cells to see if they're the same.
    * Use `isBgRGB`, `isBgPalette` and `isBgDefault` to check what color mode
    * a cell is.
    */
  def getBgColorMode(): Double
  
  /**
    * The character(s) within the cell. Examples of what this can contain:
    *
    * - A normal width character
    * - A wide character (eg. CJK)
    * - An emoji
    */
  def getChars(): String
  
  /**
    * Gets the UTF32 codepoint of single characters, if content is a combined
    * string it returns the codepoint of the last character in the string.
    */
  def getCode(): Double
  
  /**
    * Gets a cell's foreground color number, this differs depending on what the
    * color mode of the cell is:
    *
    * - Default: This should be 0, representing the default foreground color
    *   (CSI 39 m).
    * - Palette: This is a number from 0 to 255 of ANSI colors (CSI 3(0-7) m,
    *   CSI 9(0-7) m, CSI 38 ; 5 ; 0-255 m).
    * - RGB: A hex value representing a 'true color': 0xRRGGBB.
    *   (CSI 3 8 ; 2 ; Pi ; Pr ; Pg ; Pb)
    */
  def getFgColor(): Double
  
  /**
    * Gets the number representation of the foreground color mode, this can be
    * used to perform quick comparisons of 2 cells to see if they're the same.
    * Use `isFgRGB`, `isFgPalette` and `isFgDefault` to check what color mode
    * a cell is.
    */
  def getFgColorMode(): Double
  
  /**
    * The width of the character. Some examples:
    *
    * - `1` for most cells.
    * - `2` for wide character like CJK glyphs.
    * - `0` for cells immediately following cells with a width of `2`.
    */
  def getWidth(): Double
  
  /** Whether the cell has the default attribute (no color or style). */
  def isAttributeDefault(): Boolean
  
  /** Whether the cell is using the default background color mode. */
  def isBgDefault(): Boolean
  
  /** Whether the cell is using the palette background color mode. */
  def isBgPalette(): Boolean
  
  /** Whether the cell is using the RGB background color mode. */
  def isBgRGB(): Boolean
  
  /** Whether the cell has the blink attribute (CSI 5 m). */
  def isBlink(): Double
  
  /** Whether the cell has the bold attribute (CSI 1 m). */
  def isBold(): Double
  
  /** Whether the cell has the dim attribute (CSI 2 m). */
  def isDim(): Double
  
  /** Whether the cell is using the default foreground color mode. */
  def isFgDefault(): Boolean
  
  /** Whether the cell is using the palette foreground color mode. */
  def isFgPalette(): Boolean
  
  /** Whether the cell is using the RGB foreground color mode. */
  def isFgRGB(): Boolean
  
  /** Whether the cell has the inverse attribute (CSI 7 m). */
  def isInverse(): Double
  
  /** Whether the cell has the invisible attribute (CSI 8 m). */
  def isInvisible(): Double
  
  /** Whether the cell has the italic attribute (CSI 3 m). */
  def isItalic(): Double
  
  /** Whether the cell has the overline attribute (CSI 53 m). */
  def isOverline(): Double
  
  /** Whether the cell has the strikethrough attribute (CSI 9 m). */
  def isStrikethrough(): Double
  
  /** Whether the cell has the underline attribute (CSI 4 m). */
  def isUnderline(): Double
}
object IBufferCell {
  
  inline def apply(
    getBgColor: () => Double,
    getBgColorMode: () => Double,
    getChars: () => String,
    getCode: () => Double,
    getFgColor: () => Double,
    getFgColorMode: () => Double,
    getWidth: () => Double,
    isAttributeDefault: () => Boolean,
    isBgDefault: () => Boolean,
    isBgPalette: () => Boolean,
    isBgRGB: () => Boolean,
    isBlink: () => Double,
    isBold: () => Double,
    isDim: () => Double,
    isFgDefault: () => Boolean,
    isFgPalette: () => Boolean,
    isFgRGB: () => Boolean,
    isInverse: () => Double,
    isInvisible: () => Double,
    isItalic: () => Double,
    isOverline: () => Double,
    isStrikethrough: () => Double,
    isUnderline: () => Double
  ): IBufferCell = {
    val __obj = js.Dynamic.literal(getBgColor = js.Any.fromFunction0(getBgColor), getBgColorMode = js.Any.fromFunction0(getBgColorMode), getChars = js.Any.fromFunction0(getChars), getCode = js.Any.fromFunction0(getCode), getFgColor = js.Any.fromFunction0(getFgColor), getFgColorMode = js.Any.fromFunction0(getFgColorMode), getWidth = js.Any.fromFunction0(getWidth), isAttributeDefault = js.Any.fromFunction0(isAttributeDefault), isBgDefault = js.Any.fromFunction0(isBgDefault), isBgPalette = js.Any.fromFunction0(isBgPalette), isBgRGB = js.Any.fromFunction0(isBgRGB), isBlink = js.Any.fromFunction0(isBlink), isBold = js.Any.fromFunction0(isBold), isDim = js.Any.fromFunction0(isDim), isFgDefault = js.Any.fromFunction0(isFgDefault), isFgPalette = js.Any.fromFunction0(isFgPalette), isFgRGB = js.Any.fromFunction0(isFgRGB), isInverse = js.Any.fromFunction0(isInverse), isInvisible = js.Any.fromFunction0(isInvisible), isItalic = js.Any.fromFunction0(isItalic), isOverline = js.Any.fromFunction0(isOverline), isStrikethrough = js.Any.fromFunction0(isStrikethrough), isUnderline = js.Any.fromFunction0(isUnderline))
    __obj.asInstanceOf[IBufferCell]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: IBufferCell] (val x: Self) extends AnyVal {
    
    inline def setGetBgColor(value: () => Double): Self = StObject.set(x, "getBgColor", js.Any.fromFunction0(value))
    
    inline def setGetBgColorMode(value: () => Double): Self = StObject.set(x, "getBgColorMode", js.Any.fromFunction0(value))
    
    inline def setGetChars(value: () => String): Self = StObject.set(x, "getChars", js.Any.fromFunction0(value))
    
    inline def setGetCode(value: () => Double): Self = StObject.set(x, "getCode", js.Any.fromFunction0(value))
    
    inline def setGetFgColor(value: () => Double): Self = StObject.set(x, "getFgColor", js.Any.fromFunction0(value))
    
    inline def setGetFgColorMode(value: () => Double): Self = StObject.set(x, "getFgColorMode", js.Any.fromFunction0(value))
    
    inline def setGetWidth(value: () => Double): Self = StObject.set(x, "getWidth", js.Any.fromFunction0(value))
    
    inline def setIsAttributeDefault(value: () => Boolean): Self = StObject.set(x, "isAttributeDefault", js.Any.fromFunction0(value))
    
    inline def setIsBgDefault(value: () => Boolean): Self = StObject.set(x, "isBgDefault", js.Any.fromFunction0(value))
    
    inline def setIsBgPalette(value: () => Boolean): Self = StObject.set(x, "isBgPalette", js.Any.fromFunction0(value))
    
    inline def setIsBgRGB(value: () => Boolean): Self = StObject.set(x, "isBgRGB", js.Any.fromFunction0(value))
    
    inline def setIsBlink(value: () => Double): Self = StObject.set(x, "isBlink", js.Any.fromFunction0(value))
    
    inline def setIsBold(value: () => Double): Self = StObject.set(x, "isBold", js.Any.fromFunction0(value))
    
    inline def setIsDim(value: () => Double): Self = StObject.set(x, "isDim", js.Any.fromFunction0(value))
    
    inline def setIsFgDefault(value: () => Boolean): Self = StObject.set(x, "isFgDefault", js.Any.fromFunction0(value))
    
    inline def setIsFgPalette(value: () => Boolean): Self = StObject.set(x, "isFgPalette", js.Any.fromFunction0(value))
    
    inline def setIsFgRGB(value: () => Boolean): Self = StObject.set(x, "isFgRGB", js.Any.fromFunction0(value))
    
    inline def setIsInverse(value: () => Double): Self = StObject.set(x, "isInverse", js.Any.fromFunction0(value))
    
    inline def setIsInvisible(value: () => Double): Self = StObject.set(x, "isInvisible", js.Any.fromFunction0(value))
    
    inline def setIsItalic(value: () => Double): Self = StObject.set(x, "isItalic", js.Any.fromFunction0(value))
    
    inline def setIsOverline(value: () => Double): Self = StObject.set(x, "isOverline", js.Any.fromFunction0(value))
    
    inline def setIsStrikethrough(value: () => Double): Self = StObject.set(x, "isStrikethrough", js.Any.fromFunction0(value))
    
    inline def setIsUnderline(value: () => Double): Self = StObject.set(x, "isUnderline", js.Any.fromFunction0(value))
  }
}
