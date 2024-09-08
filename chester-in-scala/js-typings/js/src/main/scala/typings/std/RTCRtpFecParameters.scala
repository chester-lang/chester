package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait RTCRtpFecParameters extends StObject {
  
  /* standard dom */
  var mechanism: js.UndefOr[java.lang.String] = js.undefined
  
  /* standard dom */
  var ssrc: js.UndefOr[Double] = js.undefined
}
object RTCRtpFecParameters {
  
  inline def apply(): RTCRtpFecParameters = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[RTCRtpFecParameters]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: RTCRtpFecParameters] (val x: Self) extends AnyVal {
    
    inline def setMechanism(value: java.lang.String): Self = StObject.set(x, "mechanism", value.asInstanceOf[js.Any])
    
    inline def setMechanismUndefined: Self = StObject.set(x, "mechanism", js.undefined)
    
    inline def setSsrc(value: Double): Self = StObject.set(x, "ssrc", value.asInstanceOf[js.Any])
    
    inline def setSsrcUndefined: Self = StObject.set(x, "ssrc", js.undefined)
  }
}
