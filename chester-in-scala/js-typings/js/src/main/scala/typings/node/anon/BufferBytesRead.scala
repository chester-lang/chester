package typings.node.anon

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait BufferBytesRead extends StObject {
  
  var buffer: js.typedarray.Uint8ClampedArray
  
  var bytesRead: Double
}
object BufferBytesRead {
  
  inline def apply(buffer: js.typedarray.Uint8ClampedArray, bytesRead: Double): BufferBytesRead = {
    val __obj = js.Dynamic.literal(buffer = buffer.asInstanceOf[js.Any], bytesRead = bytesRead.asInstanceOf[js.Any])
    __obj.asInstanceOf[BufferBytesRead]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: BufferBytesRead] (val x: Self) extends AnyVal {
    
    inline def setBuffer(value: js.typedarray.Uint8ClampedArray): Self = StObject.set(x, "buffer", value.asInstanceOf[js.Any])
    
    inline def setBytesRead(value: Double): Self = StObject.set(x, "bytesRead", value.asInstanceOf[js.Any])
  }
}