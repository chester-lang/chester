package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait StoreSiteSpecificExceptionsInformation
  extends StObject
     with StoreExceptionsInformation {
  
  /* standard dom */
  var arrayOfDomainStrings: js.UndefOr[js.Array[java.lang.String]] = js.undefined
}
object StoreSiteSpecificExceptionsInformation {
  
  inline def apply(): StoreSiteSpecificExceptionsInformation = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[StoreSiteSpecificExceptionsInformation]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: StoreSiteSpecificExceptionsInformation] (val x: Self) extends AnyVal {
    
    inline def setArrayOfDomainStrings(value: js.Array[java.lang.String]): Self = StObject.set(x, "arrayOfDomainStrings", value.asInstanceOf[js.Any])
    
    inline def setArrayOfDomainStringsUndefined: Self = StObject.set(x, "arrayOfDomainStrings", js.undefined)
    
    inline def setArrayOfDomainStringsVarargs(value: java.lang.String*): Self = StObject.set(x, "arrayOfDomainStrings", js.Array(value*))
  }
}