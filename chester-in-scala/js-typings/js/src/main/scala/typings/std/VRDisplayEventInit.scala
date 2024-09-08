package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait VRDisplayEventInit
  extends StObject
     with EventInit {
  
  /* standard dom */
  var display: VRDisplay
  
  /* standard dom */
  var reason: js.UndefOr[VRDisplayEventReason] = js.undefined
}
object VRDisplayEventInit {
  
  inline def apply(display: VRDisplay): VRDisplayEventInit = {
    val __obj = js.Dynamic.literal(display = display.asInstanceOf[js.Any])
    __obj.asInstanceOf[VRDisplayEventInit]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: VRDisplayEventInit] (val x: Self) extends AnyVal {
    
    inline def setDisplay(value: VRDisplay): Self = StObject.set(x, "display", value.asInstanceOf[js.Any])
    
    inline def setReason(value: VRDisplayEventReason): Self = StObject.set(x, "reason", value.asInstanceOf[js.Any])
    
    inline def setReasonUndefined: Self = StObject.set(x, "reason", js.undefined)
  }
}