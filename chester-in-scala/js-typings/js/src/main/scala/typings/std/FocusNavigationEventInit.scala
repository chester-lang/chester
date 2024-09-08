package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait FocusNavigationEventInit
  extends StObject
     with EventInit {
  
  /* standard dom */
  var navigationReason: js.UndefOr[java.lang.String | Null] = js.undefined
  
  /* standard dom */
  var originHeight: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var originLeft: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var originTop: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var originWidth: js.UndefOr[Double] = js.undefined
}
object FocusNavigationEventInit {
  
  inline def apply(): FocusNavigationEventInit = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[FocusNavigationEventInit]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: FocusNavigationEventInit] (val x: Self) extends AnyVal {
    
    inline def setNavigationReason(value: java.lang.String): Self = StObject.set(x, "navigationReason", value.asInstanceOf[js.Any])
    
    inline def setNavigationReasonNull: Self = StObject.set(x, "navigationReason", null)
    
    inline def setNavigationReasonUndefined: Self = StObject.set(x, "navigationReason", js.undefined)
    
    inline def setOriginHeight(value: Double): Self = StObject.set(x, "originHeight", value.asInstanceOf[js.Any])
    
    inline def setOriginHeightUndefined: Self = StObject.set(x, "originHeight", js.undefined)
    
    inline def setOriginLeft(value: Double): Self = StObject.set(x, "originLeft", value.asInstanceOf[js.Any])
    
    inline def setOriginLeftUndefined: Self = StObject.set(x, "originLeft", js.undefined)
    
    inline def setOriginTop(value: Double): Self = StObject.set(x, "originTop", value.asInstanceOf[js.Any])
    
    inline def setOriginTopUndefined: Self = StObject.set(x, "originTop", js.undefined)
    
    inline def setOriginWidth(value: Double): Self = StObject.set(x, "originWidth", value.asInstanceOf[js.Any])
    
    inline def setOriginWidthUndefined: Self = StObject.set(x, "originWidth", js.undefined)
  }
}