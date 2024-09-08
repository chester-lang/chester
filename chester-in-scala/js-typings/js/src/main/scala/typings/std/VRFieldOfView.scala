package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/** This WebVR API interface represents a field of view defined by 4 different degree values describing the view from a center point. */
trait VRFieldOfView extends StObject {
  
  /* standard dom */
  val downDegrees: Double
  
  /* standard dom */
  val leftDegrees: Double
  
  /* standard dom */
  val rightDegrees: Double
  
  /* standard dom */
  val upDegrees: Double
}
object VRFieldOfView {
  
  inline def apply(downDegrees: Double, leftDegrees: Double, rightDegrees: Double, upDegrees: Double): VRFieldOfView = {
    val __obj = js.Dynamic.literal(downDegrees = downDegrees.asInstanceOf[js.Any], leftDegrees = leftDegrees.asInstanceOf[js.Any], rightDegrees = rightDegrees.asInstanceOf[js.Any], upDegrees = upDegrees.asInstanceOf[js.Any])
    __obj.asInstanceOf[VRFieldOfView]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: VRFieldOfView] (val x: Self) extends AnyVal {
    
    inline def setDownDegrees(value: Double): Self = StObject.set(x, "downDegrees", value.asInstanceOf[js.Any])
    
    inline def setLeftDegrees(value: Double): Self = StObject.set(x, "leftDegrees", value.asInstanceOf[js.Any])
    
    inline def setRightDegrees(value: Double): Self = StObject.set(x, "rightDegrees", value.asInstanceOf[js.Any])
    
    inline def setUpDegrees(value: Double): Self = StObject.set(x, "upDegrees", value.asInstanceOf[js.Any])
  }
}
