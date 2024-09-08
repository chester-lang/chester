package typings.undiciTypes.errorsMod.Errors

import typings.undiciTypes.undiciTypesStrings.UND_ERR_NOT_SUPPORTED
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait NotSupportedError
  extends StObject
     with UndiciError {
  
  @JSName("code")
  var code_NotSupportedError: UND_ERR_NOT_SUPPORTED
  
  @JSName("name")
  var name_NotSupportedError: typings.undiciTypes.undiciTypesStrings.NotSupportedError
}
object NotSupportedError {
  
  inline def apply(message: String): NotSupportedError = {
    val __obj = js.Dynamic.literal(code = "UND_ERR_NOT_SUPPORTED", message = message.asInstanceOf[js.Any], name = "NotSupportedError")
    __obj.asInstanceOf[NotSupportedError]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: NotSupportedError] (val x: Self) extends AnyVal {
    
    inline def setCode(value: UND_ERR_NOT_SUPPORTED): Self = StObject.set(x, "code", value.asInstanceOf[js.Any])
    
    inline def setName(value: typings.undiciTypes.undiciTypesStrings.NotSupportedError): Self = StObject.set(x, "name", value.asInstanceOf[js.Any])
  }
}
