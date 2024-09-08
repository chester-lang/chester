package typings.node.anon

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait Idle extends StObject {
  
  /** The number of milliseconds the CPU has spent in idle mode. */
  var idle: Double
  
  /** The number of milliseconds the CPU has spent in irq mode. */
  var irq: Double
  
  /** The number of milliseconds the CPU has spent in nice mode. */
  var nice: Double
  
  /** The number of milliseconds the CPU has spent in sys mode. */
  var sys: Double
  
  /** The number of milliseconds the CPU has spent in user mode. */
  var user: Double
}
object Idle {
  
  inline def apply(idle: Double, irq: Double, nice: Double, sys: Double, user: Double): Idle = {
    val __obj = js.Dynamic.literal(idle = idle.asInstanceOf[js.Any], irq = irq.asInstanceOf[js.Any], nice = nice.asInstanceOf[js.Any], sys = sys.asInstanceOf[js.Any], user = user.asInstanceOf[js.Any])
    __obj.asInstanceOf[Idle]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: Idle] (val x: Self) extends AnyVal {
    
    inline def setIdle(value: Double): Self = StObject.set(x, "idle", value.asInstanceOf[js.Any])
    
    inline def setIrq(value: Double): Self = StObject.set(x, "irq", value.asInstanceOf[js.Any])
    
    inline def setNice(value: Double): Self = StObject.set(x, "nice", value.asInstanceOf[js.Any])
    
    inline def setSys(value: Double): Self = StObject.set(x, "sys", value.asInstanceOf[js.Any])
    
    inline def setUser(value: Double): Self = StObject.set(x, "user", value.asInstanceOf[js.Any])
  }
}
