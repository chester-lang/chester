package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/** Returned by the HTMLVideoElement.getVideoPlaybackQuality() method and contains metrics that can be used to determine the playback quality of a video. */
trait VideoPlaybackQuality extends StObject {
  
  /* standard dom */
  val creationTime: Double
  
  /* standard dom */
  val droppedVideoFrames: Double
  
  /* standard dom */
  val totalVideoFrames: Double
}
object VideoPlaybackQuality {
  
  inline def apply(creationTime: Double, droppedVideoFrames: Double, totalVideoFrames: Double): VideoPlaybackQuality = {
    val __obj = js.Dynamic.literal(creationTime = creationTime.asInstanceOf[js.Any], droppedVideoFrames = droppedVideoFrames.asInstanceOf[js.Any], totalVideoFrames = totalVideoFrames.asInstanceOf[js.Any])
    __obj.asInstanceOf[VideoPlaybackQuality]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: VideoPlaybackQuality] (val x: Self) extends AnyVal {
    
    inline def setCreationTime(value: Double): Self = StObject.set(x, "creationTime", value.asInstanceOf[js.Any])
    
    inline def setDroppedVideoFrames(value: Double): Self = StObject.set(x, "droppedVideoFrames", value.asInstanceOf[js.Any])
    
    inline def setTotalVideoFrames(value: Double): Self = StObject.set(x, "totalVideoFrames", value.asInstanceOf[js.Any])
  }
}