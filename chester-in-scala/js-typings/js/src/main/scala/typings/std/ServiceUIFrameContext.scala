package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait ServiceUIFrameContext extends StObject {
  
  /* standard dom */
  def getCachedFrameMessage(key: java.lang.String): java.lang.String
  
  /* standard dom */
  def postFrameMessage(key: java.lang.String, data: java.lang.String): Unit
}
object ServiceUIFrameContext {
  
  inline def apply(
    getCachedFrameMessage: java.lang.String => java.lang.String,
    postFrameMessage: (java.lang.String, java.lang.String) => Unit
  ): ServiceUIFrameContext = {
    val __obj = js.Dynamic.literal(getCachedFrameMessage = js.Any.fromFunction1(getCachedFrameMessage), postFrameMessage = js.Any.fromFunction2(postFrameMessage))
    __obj.asInstanceOf[ServiceUIFrameContext]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: ServiceUIFrameContext] (val x: Self) extends AnyVal {
    
    inline def setGetCachedFrameMessage(value: java.lang.String => java.lang.String): Self = StObject.set(x, "getCachedFrameMessage", js.Any.fromFunction1(value))
    
    inline def setPostFrameMessage(value: (java.lang.String, java.lang.String) => Unit): Self = StObject.set(x, "postFrameMessage", js.Any.fromFunction2(value))
  }
}