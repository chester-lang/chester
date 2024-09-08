package typings.undiciTypes.anon

import typings.undiciTypes.mockInterceptorMod.MockInterceptor.MockResponseOptions
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait Data[TData /* <: js.Object */] extends StObject {
  
  var data: js.UndefOr[
    TData | (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify Buffer */ Any) | String
  ] = js.undefined
  
  var responseOptions: js.UndefOr[MockResponseOptions] = js.undefined
  
  var statusCode: Double
}
object Data {
  
  inline def apply[TData /* <: js.Object */](statusCode: Double): Data[TData] = {
    val __obj = js.Dynamic.literal(statusCode = statusCode.asInstanceOf[js.Any])
    __obj.asInstanceOf[Data[TData]]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: Data[?], TData /* <: js.Object */] (val x: Self & Data[TData]) extends AnyVal {
    
    inline def setData(
      value: TData | (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify Buffer */ Any) | String
    ): Self = StObject.set(x, "data", value.asInstanceOf[js.Any])
    
    inline def setDataUndefined: Self = StObject.set(x, "data", js.undefined)
    
    inline def setResponseOptions(value: MockResponseOptions): Self = StObject.set(x, "responseOptions", value.asInstanceOf[js.Any])
    
    inline def setResponseOptionsUndefined: Self = StObject.set(x, "responseOptions", js.undefined)
    
    inline def setStatusCode(value: Double): Self = StObject.set(x, "statusCode", value.asInstanceOf[js.Any])
  }
}
