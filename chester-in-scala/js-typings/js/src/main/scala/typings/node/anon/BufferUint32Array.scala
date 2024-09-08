package typings.node.anon

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait BufferUint32Array extends StObject {
  
  var buffer: js.typedarray.Uint32Array
  
  var bytesRead: Double
}
object BufferUint32Array {
  
  inline def apply(buffer: js.typedarray.Uint32Array, bytesRead: Double): BufferUint32Array = {
    val __obj = js.Dynamic.literal(buffer = buffer.asInstanceOf[js.Any], bytesRead = bytesRead.asInstanceOf[js.Any])
    __obj.asInstanceOf[BufferUint32Array]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: BufferUint32Array] (val x: Self) extends AnyVal {
    
    inline def setBuffer(value: js.typedarray.Uint32Array): Self = StObject.set(x, "buffer", value.asInstanceOf[js.Any])
    
    inline def setBytesRead(value: Double): Self = StObject.set(x, "bytesRead", value.asInstanceOf[js.Any])
  }
}
