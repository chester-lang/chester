package typings.xtermXterm.mod

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/**
  * A range within a buffer.
  */
trait IBufferRange extends StObject {
  
  /**
    * The end position of the range.
    */
  var end: IBufferCellPosition
  
  /**
    * The start position of the range.
    */
  var start: IBufferCellPosition
}
object IBufferRange {
  
  inline def apply(end: IBufferCellPosition, start: IBufferCellPosition): IBufferRange = {
    val __obj = js.Dynamic.literal(end = end.asInstanceOf[js.Any], start = start.asInstanceOf[js.Any])
    __obj.asInstanceOf[IBufferRange]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: IBufferRange] (val x: Self) extends AnyVal {
    
    inline def setEnd(value: IBufferCellPosition): Self = StObject.set(x, "end", value.asInstanceOf[js.Any])
    
    inline def setStart(value: IBufferCellPosition): Self = StObject.set(x, "start", value.asInstanceOf[js.Any])
  }
}
