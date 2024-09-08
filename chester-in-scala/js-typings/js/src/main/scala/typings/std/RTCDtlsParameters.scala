package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait RTCDtlsParameters extends StObject {
  
  /* standard dom */
  var fingerprints: js.UndefOr[js.Array[RTCDtlsFingerprint]] = js.undefined
  
  /* standard dom */
  var role: js.UndefOr[RTCDtlsRole] = js.undefined
}
object RTCDtlsParameters {
  
  inline def apply(): RTCDtlsParameters = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[RTCDtlsParameters]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: RTCDtlsParameters] (val x: Self) extends AnyVal {
    
    inline def setFingerprints(value: js.Array[RTCDtlsFingerprint]): Self = StObject.set(x, "fingerprints", value.asInstanceOf[js.Any])
    
    inline def setFingerprintsUndefined: Self = StObject.set(x, "fingerprints", js.undefined)
    
    inline def setFingerprintsVarargs(value: RTCDtlsFingerprint*): Self = StObject.set(x, "fingerprints", js.Array(value*))
    
    inline def setRole(value: RTCDtlsRole): Self = StObject.set(x, "role", value.asInstanceOf[js.Any])
    
    inline def setRoleUndefined: Self = StObject.set(x, "role", js.undefined)
  }
}
