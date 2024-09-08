package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait MSFIDOCredentialAssertion
  extends StObject
     with MSAssertion {
  
  /* standard dom */
  val algorithm: java.lang.String | org.scalajs.dom.Algorithm
  
  /* standard dom */
  val attestation: Any
  
  /* standard dom */
  val publicKey: java.lang.String
  
  /* standard dom */
  val transportHints: js.Array[MSTransportType]
}
object MSFIDOCredentialAssertion {
  
  inline def apply(
    algorithm: java.lang.String | org.scalajs.dom.Algorithm,
    attestation: Any,
    id: java.lang.String,
    publicKey: java.lang.String,
    transportHints: js.Array[MSTransportType],
    `type`: MSCredentialType
  ): MSFIDOCredentialAssertion = {
    val __obj = js.Dynamic.literal(algorithm = algorithm.asInstanceOf[js.Any], attestation = attestation.asInstanceOf[js.Any], id = id.asInstanceOf[js.Any], publicKey = publicKey.asInstanceOf[js.Any], transportHints = transportHints.asInstanceOf[js.Any])
    __obj.updateDynamic("type")(`type`.asInstanceOf[js.Any])
    __obj.asInstanceOf[MSFIDOCredentialAssertion]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: MSFIDOCredentialAssertion] (val x: Self) extends AnyVal {
    
    inline def setAlgorithm(value: java.lang.String | org.scalajs.dom.Algorithm): Self = StObject.set(x, "algorithm", value.asInstanceOf[js.Any])
    
    inline def setAttestation(value: Any): Self = StObject.set(x, "attestation", value.asInstanceOf[js.Any])
    
    inline def setPublicKey(value: java.lang.String): Self = StObject.set(x, "publicKey", value.asInstanceOf[js.Any])
    
    inline def setTransportHints(value: js.Array[MSTransportType]): Self = StObject.set(x, "transportHints", value.asInstanceOf[js.Any])
    
    inline def setTransportHintsVarargs(value: MSTransportType*): Self = StObject.set(x, "transportHints", js.Array(value*))
  }
}
