package typings.undiciTypes.errorsMod.Errors

import typings.undiciTypes.undiciTypesStrings.UND_ERR_INFO
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait InformationalError
  extends StObject
     with UndiciError {
  
  @JSName("code")
  var code_InformationalError: UND_ERR_INFO
  
  @JSName("name")
  var name_InformationalError: typings.undiciTypes.undiciTypesStrings.InformationalError
}
object InformationalError {
  
  inline def apply(message: String): InformationalError = {
    val __obj = js.Dynamic.literal(code = "UND_ERR_INFO", message = message.asInstanceOf[js.Any], name = "InformationalError")
    __obj.asInstanceOf[InformationalError]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: InformationalError] (val x: Self) extends AnyVal {
    
    inline def setCode(value: UND_ERR_INFO): Self = StObject.set(x, "code", value.asInstanceOf[js.Any])
    
    inline def setName(value: typings.undiciTypes.undiciTypesStrings.InformationalError): Self = StObject.set(x, "name", value.asInstanceOf[js.Any])
  }
}
