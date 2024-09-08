package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait SpeechRecognitionEventInit
  extends StObject
     with EventInit {
  
  /* standard dom */
  var resultIndex: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var results: SpeechRecognitionResultList
}
object SpeechRecognitionEventInit {
  
  inline def apply(results: SpeechRecognitionResultList): SpeechRecognitionEventInit = {
    val __obj = js.Dynamic.literal(results = results.asInstanceOf[js.Any])
    __obj.asInstanceOf[SpeechRecognitionEventInit]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: SpeechRecognitionEventInit] (val x: Self) extends AnyVal {
    
    inline def setResultIndex(value: Double): Self = StObject.set(x, "resultIndex", value.asInstanceOf[js.Any])
    
    inline def setResultIndexUndefined: Self = StObject.set(x, "resultIndex", js.undefined)
    
    inline def setResults(value: SpeechRecognitionResultList): Self = StObject.set(x, "results", value.asInstanceOf[js.Any])
  }
}