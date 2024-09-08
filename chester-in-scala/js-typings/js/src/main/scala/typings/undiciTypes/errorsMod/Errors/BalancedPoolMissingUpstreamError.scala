package typings.undiciTypes.errorsMod.Errors

import typings.undiciTypes.undiciTypesStrings.MissingUpstreamError
import typings.undiciTypes.undiciTypesStrings.UND_ERR_BPL_MISSING_UPSTREAM
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait BalancedPoolMissingUpstreamError
  extends StObject
     with UndiciError {
  
  @JSName("code")
  var code_BalancedPoolMissingUpstreamError: UND_ERR_BPL_MISSING_UPSTREAM
  
  @JSName("name")
  var name_BalancedPoolMissingUpstreamError: MissingUpstreamError
}
object BalancedPoolMissingUpstreamError {
  
  inline def apply(message: String): BalancedPoolMissingUpstreamError = {
    val __obj = js.Dynamic.literal(code = "UND_ERR_BPL_MISSING_UPSTREAM", message = message.asInstanceOf[js.Any], name = "MissingUpstreamError")
    __obj.asInstanceOf[BalancedPoolMissingUpstreamError]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: BalancedPoolMissingUpstreamError] (val x: Self) extends AnyVal {
    
    inline def setCode(value: UND_ERR_BPL_MISSING_UPSTREAM): Self = StObject.set(x, "code", value.asInstanceOf[js.Any])
    
    inline def setName(value: MissingUpstreamError): Self = StObject.set(x, "name", value.asInstanceOf[js.Any])
  }
}