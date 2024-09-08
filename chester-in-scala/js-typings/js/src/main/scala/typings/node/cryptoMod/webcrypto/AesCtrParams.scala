package typings.node.cryptoMod.webcrypto

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait AesCtrParams
  extends StObject
     with Algorithm {
  
  var counter: BufferSource
  
  var length: Double
}
object AesCtrParams {
  
  inline def apply(counter: BufferSource, length: Double, name: String): AesCtrParams = {
    val __obj = js.Dynamic.literal(counter = counter.asInstanceOf[js.Any], length = length.asInstanceOf[js.Any], name = name.asInstanceOf[js.Any])
    __obj.asInstanceOf[AesCtrParams]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: AesCtrParams] (val x: Self) extends AnyVal {
    
    inline def setCounter(value: BufferSource): Self = StObject.set(x, "counter", value.asInstanceOf[js.Any])
    
    inline def setLength(value: Double): Self = StObject.set(x, "length", value.asInstanceOf[js.Any])
  }
}
