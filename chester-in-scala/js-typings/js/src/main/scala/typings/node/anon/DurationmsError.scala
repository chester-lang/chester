package typings.node.anon

import typings.node.TestError
import typings.node.nodeStrings.suite
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait DurationmsError extends StObject {
  
  /**
    * The duration of the test in milliseconds.
    */
  var duration_ms: Double
  
  /**
    * An error wrapping the error thrown by the test.
    */
  var error: TestError
  
  /**
    * The type of the test, used to denote whether this is a suite.
    * @since v20.0.0, v19.9.0, v18.17.0
    */
  var `type`: js.UndefOr[suite] = js.undefined
}
object DurationmsError {
  
  inline def apply(duration_ms: Double, error: TestError): DurationmsError = {
    val __obj = js.Dynamic.literal(duration_ms = duration_ms.asInstanceOf[js.Any], error = error.asInstanceOf[js.Any])
    __obj.asInstanceOf[DurationmsError]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: DurationmsError] (val x: Self) extends AnyVal {
    
    inline def setDuration_ms(value: Double): Self = StObject.set(x, "duration_ms", value.asInstanceOf[js.Any])
    
    inline def setError(value: TestError): Self = StObject.set(x, "error", value.asInstanceOf[js.Any])
    
    inline def setType(value: suite): Self = StObject.set(x, "type", value.asInstanceOf[js.Any])
    
    inline def setTypeUndefined: Self = StObject.set(x, "type", js.undefined)
  }
}