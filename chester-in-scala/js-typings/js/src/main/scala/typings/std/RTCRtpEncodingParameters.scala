package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait RTCRtpEncodingParameters
  extends StObject
     with RTCRtpCodingParameters {
  
  /* standard dom */
  var active: js.UndefOr[scala.Boolean] = js.undefined
  
  /* standard dom */
  var maxBitrate: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var scaleResolutionDownBy: js.UndefOr[Double] = js.undefined
}
object RTCRtpEncodingParameters {
  
  inline def apply(): RTCRtpEncodingParameters = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[RTCRtpEncodingParameters]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: RTCRtpEncodingParameters] (val x: Self) extends AnyVal {
    
    inline def setActive(value: scala.Boolean): Self = StObject.set(x, "active", value.asInstanceOf[js.Any])
    
    inline def setActiveUndefined: Self = StObject.set(x, "active", js.undefined)
    
    inline def setMaxBitrate(value: Double): Self = StObject.set(x, "maxBitrate", value.asInstanceOf[js.Any])
    
    inline def setMaxBitrateUndefined: Self = StObject.set(x, "maxBitrate", js.undefined)
    
    inline def setScaleResolutionDownBy(value: Double): Self = StObject.set(x, "scaleResolutionDownBy", value.asInstanceOf[js.Any])
    
    inline def setScaleResolutionDownByUndefined: Self = StObject.set(x, "scaleResolutionDownBy", js.undefined)
  }
}
