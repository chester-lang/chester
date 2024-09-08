package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/** This Gamepad API interface defines an individual gamepad or other controller, allowing access to information such as button presses, axis positions, and id. */
trait Gamepad extends StObject {
  
  /* standard dom */
  val axes: js.Array[Double]
  
  /* standard dom */
  val buttons: js.Array[org.scalajs.dom.GamepadButton]
  
  /* standard dom */
  val connected: scala.Boolean
  
  /* standard dom */
  val hand: GamepadHand
  
  /* standard dom */
  val hapticActuators: js.Array[GamepadHapticActuator]
  
  /* standard dom */
  val id: java.lang.String
  
  /* standard dom */
  val index: Double
  
  /* standard dom */
  val mapping: org.scalajs.dom.GamepadMappingType
  
  /* standard dom */
  val pose: GamepadPose | Null
  
  /* standard dom */
  val timestamp: Double
}
object Gamepad {
  
  inline def apply(
    axes: js.Array[Double],
    buttons: js.Array[org.scalajs.dom.GamepadButton],
    connected: scala.Boolean,
    hand: GamepadHand,
    hapticActuators: js.Array[GamepadHapticActuator],
    id: java.lang.String,
    index: Double,
    mapping: org.scalajs.dom.GamepadMappingType,
    timestamp: Double
  ): Gamepad = {
    val __obj = js.Dynamic.literal(axes = axes.asInstanceOf[js.Any], buttons = buttons.asInstanceOf[js.Any], connected = connected.asInstanceOf[js.Any], hand = hand.asInstanceOf[js.Any], hapticActuators = hapticActuators.asInstanceOf[js.Any], id = id.asInstanceOf[js.Any], index = index.asInstanceOf[js.Any], mapping = mapping.asInstanceOf[js.Any], timestamp = timestamp.asInstanceOf[js.Any], pose = null)
    __obj.asInstanceOf[Gamepad]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: Gamepad] (val x: Self) extends AnyVal {
    
    inline def setAxes(value: js.Array[Double]): Self = StObject.set(x, "axes", value.asInstanceOf[js.Any])
    
    inline def setAxesVarargs(value: Double*): Self = StObject.set(x, "axes", js.Array(value*))
    
    inline def setButtons(value: js.Array[org.scalajs.dom.GamepadButton]): Self = StObject.set(x, "buttons", value.asInstanceOf[js.Any])
    
    inline def setButtonsVarargs(value: org.scalajs.dom.GamepadButton*): Self = StObject.set(x, "buttons", js.Array(value*))
    
    inline def setConnected(value: scala.Boolean): Self = StObject.set(x, "connected", value.asInstanceOf[js.Any])
    
    inline def setHand(value: GamepadHand): Self = StObject.set(x, "hand", value.asInstanceOf[js.Any])
    
    inline def setHapticActuators(value: js.Array[GamepadHapticActuator]): Self = StObject.set(x, "hapticActuators", value.asInstanceOf[js.Any])
    
    inline def setHapticActuatorsVarargs(value: GamepadHapticActuator*): Self = StObject.set(x, "hapticActuators", js.Array(value*))
    
    inline def setId(value: java.lang.String): Self = StObject.set(x, "id", value.asInstanceOf[js.Any])
    
    inline def setIndex(value: Double): Self = StObject.set(x, "index", value.asInstanceOf[js.Any])
    
    inline def setMapping(value: org.scalajs.dom.GamepadMappingType): Self = StObject.set(x, "mapping", value.asInstanceOf[js.Any])
    
    inline def setPose(value: GamepadPose): Self = StObject.set(x, "pose", value.asInstanceOf[js.Any])
    
    inline def setPoseNull: Self = StObject.set(x, "pose", null)
    
    inline def setTimestamp(value: Double): Self = StObject.set(x, "timestamp", value.asInstanceOf[js.Any])
  }
}
