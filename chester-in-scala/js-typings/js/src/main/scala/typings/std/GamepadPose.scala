package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/** This Gamepad API interface represents the pose of a WebVR controller at a given timestamp (which includes orientation, position, velocity, and acceleration information.) */
trait GamepadPose extends StObject {
  
  /* standard dom */
  val angularAcceleration: js.typedarray.Float32Array | Null
  
  /* standard dom */
  val angularVelocity: js.typedarray.Float32Array | Null
  
  /* standard dom */
  val hasOrientation: scala.Boolean
  
  /* standard dom */
  val hasPosition: scala.Boolean
  
  /* standard dom */
  val linearAcceleration: js.typedarray.Float32Array | Null
  
  /* standard dom */
  val linearVelocity: js.typedarray.Float32Array | Null
  
  /* standard dom */
  val orientation: js.typedarray.Float32Array | Null
  
  /* standard dom */
  val position: js.typedarray.Float32Array | Null
}
object GamepadPose {
  
  inline def apply(hasOrientation: scala.Boolean, hasPosition: scala.Boolean): GamepadPose = {
    val __obj = js.Dynamic.literal(hasOrientation = hasOrientation.asInstanceOf[js.Any], hasPosition = hasPosition.asInstanceOf[js.Any], angularAcceleration = null, angularVelocity = null, linearAcceleration = null, linearVelocity = null, orientation = null, position = null)
    __obj.asInstanceOf[GamepadPose]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: GamepadPose] (val x: Self) extends AnyVal {
    
    inline def setAngularAcceleration(value: js.typedarray.Float32Array): Self = StObject.set(x, "angularAcceleration", value.asInstanceOf[js.Any])
    
    inline def setAngularAccelerationNull: Self = StObject.set(x, "angularAcceleration", null)
    
    inline def setAngularVelocity(value: js.typedarray.Float32Array): Self = StObject.set(x, "angularVelocity", value.asInstanceOf[js.Any])
    
    inline def setAngularVelocityNull: Self = StObject.set(x, "angularVelocity", null)
    
    inline def setHasOrientation(value: scala.Boolean): Self = StObject.set(x, "hasOrientation", value.asInstanceOf[js.Any])
    
    inline def setHasPosition(value: scala.Boolean): Self = StObject.set(x, "hasPosition", value.asInstanceOf[js.Any])
    
    inline def setLinearAcceleration(value: js.typedarray.Float32Array): Self = StObject.set(x, "linearAcceleration", value.asInstanceOf[js.Any])
    
    inline def setLinearAccelerationNull: Self = StObject.set(x, "linearAcceleration", null)
    
    inline def setLinearVelocity(value: js.typedarray.Float32Array): Self = StObject.set(x, "linearVelocity", value.asInstanceOf[js.Any])
    
    inline def setLinearVelocityNull: Self = StObject.set(x, "linearVelocity", null)
    
    inline def setOrientation(value: js.typedarray.Float32Array): Self = StObject.set(x, "orientation", value.asInstanceOf[js.Any])
    
    inline def setOrientationNull: Self = StObject.set(x, "orientation", null)
    
    inline def setPosition(value: js.typedarray.Float32Array): Self = StObject.set(x, "position", value.asInstanceOf[js.Any])
    
    inline def setPositionNull: Self = StObject.set(x, "position", null)
  }
}