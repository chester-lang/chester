package typings.node.netMod

import typings.node.dnsMod.LookupAddress
import typings.node.dnsMod.LookupOptions
import typings.node.globalsMod.global.NodeJS.ErrnoException
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait TcpSocketConnectOpts
  extends StObject
     with ConnectOpts
     with SocketConnectOpts {
  
  /**
    * @since v18.13.0
    */
  var autoSelectFamily: js.UndefOr[Boolean] = js.undefined
  
  /**
    * @since v18.13.0
    */
  var autoSelectFamilyAttemptTimeout: js.UndefOr[Double] = js.undefined
  
  var family: js.UndefOr[Double] = js.undefined
  
  var hints: js.UndefOr[Double] = js.undefined
  
  var host: js.UndefOr[String] = js.undefined
  
  var keepAlive: js.UndefOr[Boolean] = js.undefined
  
  var keepAliveInitialDelay: js.UndefOr[Double] = js.undefined
  
  var localAddress: js.UndefOr[String] = js.undefined
  
  var localPort: js.UndefOr[Double] = js.undefined
  
  var lookup: js.UndefOr[LookupFunction] = js.undefined
  
  var noDelay: js.UndefOr[Boolean] = js.undefined
  
  var port: Double
}
object TcpSocketConnectOpts {
  
  inline def apply(port: Double): TcpSocketConnectOpts = {
    val __obj = js.Dynamic.literal(port = port.asInstanceOf[js.Any])
    __obj.asInstanceOf[TcpSocketConnectOpts]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: TcpSocketConnectOpts] (val x: Self) extends AnyVal {
    
    inline def setAutoSelectFamily(value: Boolean): Self = StObject.set(x, "autoSelectFamily", value.asInstanceOf[js.Any])
    
    inline def setAutoSelectFamilyAttemptTimeout(value: Double): Self = StObject.set(x, "autoSelectFamilyAttemptTimeout", value.asInstanceOf[js.Any])
    
    inline def setAutoSelectFamilyAttemptTimeoutUndefined: Self = StObject.set(x, "autoSelectFamilyAttemptTimeout", js.undefined)
    
    inline def setAutoSelectFamilyUndefined: Self = StObject.set(x, "autoSelectFamily", js.undefined)
    
    inline def setFamily(value: Double): Self = StObject.set(x, "family", value.asInstanceOf[js.Any])
    
    inline def setFamilyUndefined: Self = StObject.set(x, "family", js.undefined)
    
    inline def setHints(value: Double): Self = StObject.set(x, "hints", value.asInstanceOf[js.Any])
    
    inline def setHintsUndefined: Self = StObject.set(x, "hints", js.undefined)
    
    inline def setHost(value: String): Self = StObject.set(x, "host", value.asInstanceOf[js.Any])
    
    inline def setHostUndefined: Self = StObject.set(x, "host", js.undefined)
    
    inline def setKeepAlive(value: Boolean): Self = StObject.set(x, "keepAlive", value.asInstanceOf[js.Any])
    
    inline def setKeepAliveInitialDelay(value: Double): Self = StObject.set(x, "keepAliveInitialDelay", value.asInstanceOf[js.Any])
    
    inline def setKeepAliveInitialDelayUndefined: Self = StObject.set(x, "keepAliveInitialDelay", js.undefined)
    
    inline def setKeepAliveUndefined: Self = StObject.set(x, "keepAlive", js.undefined)
    
    inline def setLocalAddress(value: String): Self = StObject.set(x, "localAddress", value.asInstanceOf[js.Any])
    
    inline def setLocalAddressUndefined: Self = StObject.set(x, "localAddress", js.undefined)
    
    inline def setLocalPort(value: Double): Self = StObject.set(x, "localPort", value.asInstanceOf[js.Any])
    
    inline def setLocalPortUndefined: Self = StObject.set(x, "localPort", js.undefined)
    
    inline def setLookup(
      value: (/* hostname */ String, /* options */ LookupOptions, /* callback */ js.Function3[
          /* err */ ErrnoException | Null, 
          /* address */ String | js.Array[LookupAddress], 
          /* family */ js.UndefOr[Double], 
          Unit
        ]) => Unit
    ): Self = StObject.set(x, "lookup", js.Any.fromFunction3(value))
    
    inline def setLookupUndefined: Self = StObject.set(x, "lookup", js.undefined)
    
    inline def setNoDelay(value: Boolean): Self = StObject.set(x, "noDelay", value.asInstanceOf[js.Any])
    
    inline def setNoDelayUndefined: Self = StObject.set(x, "noDelay", js.undefined)
    
    inline def setPort(value: Double): Self = StObject.set(x, "port", value.asInstanceOf[js.Any])
  }
}