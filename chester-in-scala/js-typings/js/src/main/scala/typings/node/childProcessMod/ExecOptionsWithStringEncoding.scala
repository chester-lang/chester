package typings.node.childProcessMod

import typings.node.bufferMod.global.BufferEncoding
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait ExecOptionsWithStringEncoding
  extends StObject
     with ExecOptions {
  
  var encoding: BufferEncoding
}
object ExecOptionsWithStringEncoding {
  
  inline def apply(encoding: BufferEncoding): ExecOptionsWithStringEncoding = {
    val __obj = js.Dynamic.literal(encoding = encoding.asInstanceOf[js.Any])
    __obj.asInstanceOf[ExecOptionsWithStringEncoding]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: ExecOptionsWithStringEncoding] (val x: Self) extends AnyVal {
    
    inline def setEncoding(value: BufferEncoding): Self = StObject.set(x, "encoding", value.asInstanceOf[js.Any])
  }
}
