package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait MediaStreamConstraints extends StObject {
  
  /* standard dom */
  var audio: js.UndefOr[scala.Boolean | org.scalajs.dom.MediaTrackConstraints] = js.undefined
  
  /* standard dom */
  var peerIdentity: js.UndefOr[java.lang.String] = js.undefined
  
  /* standard dom */
  var video: js.UndefOr[scala.Boolean | org.scalajs.dom.MediaTrackConstraints] = js.undefined
}
object MediaStreamConstraints {
  
  inline def apply(): MediaStreamConstraints = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[MediaStreamConstraints]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: MediaStreamConstraints] (val x: Self) extends AnyVal {
    
    inline def setAudio(value: scala.Boolean | org.scalajs.dom.MediaTrackConstraints): Self = StObject.set(x, "audio", value.asInstanceOf[js.Any])
    
    inline def setAudioUndefined: Self = StObject.set(x, "audio", js.undefined)
    
    inline def setPeerIdentity(value: java.lang.String): Self = StObject.set(x, "peerIdentity", value.asInstanceOf[js.Any])
    
    inline def setPeerIdentityUndefined: Self = StObject.set(x, "peerIdentity", js.undefined)
    
    inline def setVideo(value: scala.Boolean | org.scalajs.dom.MediaTrackConstraints): Self = StObject.set(x, "video", value.asInstanceOf[js.Any])
    
    inline def setVideoUndefined: Self = StObject.set(x, "video", js.undefined)
  }
}
