package typings.node.tlsMod

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait SecureContext extends StObject {
  
  var context: Any
}
object SecureContext {
  
  inline def apply(context: Any): SecureContext = {
    val __obj = js.Dynamic.literal(context = context.asInstanceOf[js.Any])
    __obj.asInstanceOf[SecureContext]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: SecureContext] (val x: Self) extends AnyVal {
    
    inline def setContext(value: Any): Self = StObject.set(x, "context", value.asInstanceOf[js.Any])
  }
}
