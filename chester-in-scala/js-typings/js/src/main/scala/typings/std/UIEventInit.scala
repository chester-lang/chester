package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait UIEventInit
  extends StObject
     with EventInit {
  
  /* standard dom */
  var detail: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var view: js.UndefOr[org.scalajs.dom.Window | Null] = js.undefined
}
object UIEventInit {
  
  inline def apply(): UIEventInit = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[UIEventInit]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: UIEventInit] (val x: Self) extends AnyVal {
    
    inline def setDetail(value: Double): Self = StObject.set(x, "detail", value.asInstanceOf[js.Any])
    
    inline def setDetailUndefined: Self = StObject.set(x, "detail", js.undefined)
    
    inline def setView(value: org.scalajs.dom.Window): Self = StObject.set(x, "view", value.asInstanceOf[js.Any])
    
    inline def setViewNull: Self = StObject.set(x, "view", null)
    
    inline def setViewUndefined: Self = StObject.set(x, "view", js.undefined)
  }
}