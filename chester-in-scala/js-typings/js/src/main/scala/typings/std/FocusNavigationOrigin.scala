package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait FocusNavigationOrigin extends StObject {
  
  /* standard dom */
  var originHeight: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var originLeft: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var originTop: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var originWidth: js.UndefOr[Double] = js.undefined
}
object FocusNavigationOrigin {
  
  inline def apply(): FocusNavigationOrigin = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[FocusNavigationOrigin]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: FocusNavigationOrigin] (val x: Self) extends AnyVal {
    
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