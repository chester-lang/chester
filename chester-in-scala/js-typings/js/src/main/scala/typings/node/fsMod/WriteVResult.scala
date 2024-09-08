package typings.node.fsMod

import typings.node.globalsMod.global.NodeJS.ArrayBufferView
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait WriteVResult extends StObject {
  
  var buffers: js.Array[ArrayBufferView]
  
  var bytesWritten: Double
}
object WriteVResult {
  
  inline def apply(buffers: js.Array[ArrayBufferView], bytesWritten: Double): WriteVResult = {
    val __obj = js.Dynamic.literal(buffers = buffers.asInstanceOf[js.Any], bytesWritten = bytesWritten.asInstanceOf[js.Any])
    __obj.asInstanceOf[WriteVResult]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: WriteVResult] (val x: Self) extends AnyVal {
    
    inline def setBuffers(value: js.Array[ArrayBufferView]): Self = StObject.set(x, "buffers", value.asInstanceOf[js.Any])
    
    inline def setBuffersVarargs(value: ArrayBufferView*): Self = StObject.set(x, "buffers", js.Array(value*))
    
    inline def setBytesWritten(value: Double): Self = StObject.set(x, "bytesWritten", value.asInstanceOf[js.Any])
  }
}
