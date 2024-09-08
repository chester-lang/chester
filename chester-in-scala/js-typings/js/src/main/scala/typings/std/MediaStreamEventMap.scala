package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait MediaStreamEventMap extends StObject {
  
  /* standard dom */
  var addtrack: org.scalajs.dom.MediaStreamTrackEvent
  
  /* standard dom */
  var removetrack: org.scalajs.dom.MediaStreamTrackEvent
}
object MediaStreamEventMap {
  
  inline def apply(
    addtrack: org.scalajs.dom.MediaStreamTrackEvent,
    removetrack: org.scalajs.dom.MediaStreamTrackEvent
  ): MediaStreamEventMap = {
    val __obj = js.Dynamic.literal(addtrack = addtrack.asInstanceOf[js.Any], removetrack = removetrack.asInstanceOf[js.Any])
    __obj.asInstanceOf[MediaStreamEventMap]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: MediaStreamEventMap] (val x: Self) extends AnyVal {
    
    inline def setAddtrack(value: org.scalajs.dom.MediaStreamTrackEvent): Self = StObject.set(x, "addtrack", value.asInstanceOf[js.Any])
    
    inline def setRemovetrack(value: org.scalajs.dom.MediaStreamTrackEvent): Self = StObject.set(x, "removetrack", value.asInstanceOf[js.Any])
  }
}
