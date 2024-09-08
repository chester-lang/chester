package typings.node.cryptoMod

import typings.node.anon.Format
import typings.node.anon.`1`
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait ECKeyPairOptions[PubF /* <: KeyFormat */, PrivF /* <: KeyFormat */]
  extends StObject
     with ECKeyPairKeyObjectOptions {
  
  var privateKeyEncoding: BasePrivateKeyEncodingOptions[PrivF] & `1`
  
  var publicKeyEncoding: Format[PubF]
}
object ECKeyPairOptions {
  
  inline def apply[PubF /* <: KeyFormat */, PrivF /* <: KeyFormat */](
    namedCurve: String,
    privateKeyEncoding: BasePrivateKeyEncodingOptions[PrivF] & `1`,
    publicKeyEncoding: Format[PubF]
  ): ECKeyPairOptions[PubF, PrivF] = {
    val __obj = js.Dynamic.literal(namedCurve = namedCurve.asInstanceOf[js.Any], privateKeyEncoding = privateKeyEncoding.asInstanceOf[js.Any], publicKeyEncoding = publicKeyEncoding.asInstanceOf[js.Any])
    __obj.asInstanceOf[ECKeyPairOptions[PubF, PrivF]]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: ECKeyPairOptions[?, ?], PubF /* <: KeyFormat */, PrivF /* <: KeyFormat */] (val x: Self & (ECKeyPairOptions[PubF, PrivF])) extends AnyVal {
    
    inline def setPrivateKeyEncoding(value: BasePrivateKeyEncodingOptions[PrivF] & `1`): Self = StObject.set(x, "privateKeyEncoding", value.asInstanceOf[js.Any])
    
    inline def setPublicKeyEncoding(value: Format[PubF]): Self = StObject.set(x, "publicKeyEncoding", value.asInstanceOf[js.Any])
  }
}
