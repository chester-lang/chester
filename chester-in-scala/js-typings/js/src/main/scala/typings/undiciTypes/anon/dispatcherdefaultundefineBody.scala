package typings.undiciTypes.anon

import typings.std.Record
import typings.undiciTypes.dispatcherMod.AbortSignal
import typings.undiciTypes.dispatcherMod.Dispatcher.HttpMethod
import typings.undiciTypes.dispatcherMod.default
import typings.undiciTypes.formdataMod.FormData
import typings.undiciTypes.headerMod.IncomingHttpHeaders
import typings.undiciTypes.undiciTypesStrings.raw
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/* Inlined {  dispatcher :undici-types.undici-types/dispatcher.default | undefined} & std.Omit<undici-types.undici-types/dispatcher.default.PipelineOptions, 'origin' | 'path'> */
trait dispatcherdefaultundefineBody extends StObject {
  
  var blocking: js.UndefOr[Boolean] = js.undefined
  
  var body: js.UndefOr[
    String | (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify Buffer */ Any) | js.typedarray.Uint8Array | Null | FormData
  ] = js.undefined
  
  var bodyTimeout: js.UndefOr[Double | Null] = js.undefined
  
  var dispatcher: js.UndefOr[default] = js.undefined
  
  var expectContinue: js.UndefOr[Boolean] = js.undefined
  
  var headers: js.UndefOr[
    IncomingHttpHeaders | js.Array[String] | (js.Iterable[js.Tuple2[String, js.UndefOr[String | js.Array[String]]]]) | Null
  ] = js.undefined
  
  var headersTimeout: js.UndefOr[Double | Null] = js.undefined
  
  var highWaterMark: js.UndefOr[Double] = js.undefined
  
  var idempotent: js.UndefOr[Boolean] = js.undefined
  
  var maxRedirections: js.UndefOr[Double] = js.undefined
  
  var method: HttpMethod
  
  var objectMode: js.UndefOr[Boolean] = js.undefined
  
  var onInfo: js.UndefOr[js.Function1[/* info */ Headers, Unit]] = js.undefined
  
  var opaque: js.UndefOr[Any] = js.undefined
  
  var query: js.UndefOr[Record[String, Any]] = js.undefined
  
  var redirectionLimitReached: js.UndefOr[Boolean] = js.undefined
  
  var reset: js.UndefOr[Boolean] = js.undefined
  
  var responseHeader: js.UndefOr[raw | Null] = js.undefined
  
  var signal: js.UndefOr[
    AbortSignal | (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify EventEmitter */ Any) | Null
  ] = js.undefined
  
  var throwOnError: js.UndefOr[Boolean] = js.undefined
  
  var upgrade: js.UndefOr[Boolean | String | Null] = js.undefined
}
object dispatcherdefaultundefineBody {
  
  inline def apply(method: HttpMethod): dispatcherdefaultundefineBody = {
    val __obj = js.Dynamic.literal(method = method.asInstanceOf[js.Any])
    __obj.asInstanceOf[dispatcherdefaultundefineBody]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: dispatcherdefaultundefineBody] (val x: Self) extends AnyVal {
    
    inline def setBlocking(value: Boolean): Self = StObject.set(x, "blocking", value.asInstanceOf[js.Any])
    
    inline def setBlockingUndefined: Self = StObject.set(x, "blocking", js.undefined)
    
    inline def setBody(
      value: String | (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify Buffer */ Any) | js.typedarray.Uint8Array | FormData
    ): Self = StObject.set(x, "body", value.asInstanceOf[js.Any])
    
    inline def setBodyNull: Self = StObject.set(x, "body", null)
    
    inline def setBodyTimeout(value: Double): Self = StObject.set(x, "bodyTimeout", value.asInstanceOf[js.Any])
    
    inline def setBodyTimeoutNull: Self = StObject.set(x, "bodyTimeout", null)
    
    inline def setBodyTimeoutUndefined: Self = StObject.set(x, "bodyTimeout", js.undefined)
    
    inline def setBodyUndefined: Self = StObject.set(x, "body", js.undefined)
    
    inline def setDispatcher(value: default): Self = StObject.set(x, "dispatcher", value.asInstanceOf[js.Any])
    
    inline def setDispatcherUndefined: Self = StObject.set(x, "dispatcher", js.undefined)
    
    inline def setExpectContinue(value: Boolean): Self = StObject.set(x, "expectContinue", value.asInstanceOf[js.Any])
    
    inline def setExpectContinueUndefined: Self = StObject.set(x, "expectContinue", js.undefined)
    
    inline def setHeaders(
      value: IncomingHttpHeaders | js.Array[String] | (js.Iterable[js.Tuple2[String, js.UndefOr[String | js.Array[String]]]])
    ): Self = StObject.set(x, "headers", value.asInstanceOf[js.Any])
    
    inline def setHeadersNull: Self = StObject.set(x, "headers", null)
    
    inline def setHeadersTimeout(value: Double): Self = StObject.set(x, "headersTimeout", value.asInstanceOf[js.Any])
    
    inline def setHeadersTimeoutNull: Self = StObject.set(x, "headersTimeout", null)
    
    inline def setHeadersTimeoutUndefined: Self = StObject.set(x, "headersTimeout", js.undefined)
    
    inline def setHeadersUndefined: Self = StObject.set(x, "headers", js.undefined)
    
    inline def setHeadersVarargs(value: String*): Self = StObject.set(x, "headers", js.Array(value*))
    
    inline def setHighWaterMark(value: Double): Self = StObject.set(x, "highWaterMark", value.asInstanceOf[js.Any])
    
    inline def setHighWaterMarkUndefined: Self = StObject.set(x, "highWaterMark", js.undefined)
    
    inline def setIdempotent(value: Boolean): Self = StObject.set(x, "idempotent", value.asInstanceOf[js.Any])
    
    inline def setIdempotentUndefined: Self = StObject.set(x, "idempotent", js.undefined)
    
    inline def setMaxRedirections(value: Double): Self = StObject.set(x, "maxRedirections", value.asInstanceOf[js.Any])
    
    inline def setMaxRedirectionsUndefined: Self = StObject.set(x, "maxRedirections", js.undefined)
    
    inline def setMethod(value: HttpMethod): Self = StObject.set(x, "method", value.asInstanceOf[js.Any])
    
    inline def setObjectMode(value: Boolean): Self = StObject.set(x, "objectMode", value.asInstanceOf[js.Any])
    
    inline def setObjectModeUndefined: Self = StObject.set(x, "objectMode", js.undefined)
    
    inline def setOnInfo(value: /* info */ Headers => Unit): Self = StObject.set(x, "onInfo", js.Any.fromFunction1(value))
    
    inline def setOnInfoUndefined: Self = StObject.set(x, "onInfo", js.undefined)
    
    inline def setOpaque(value: Any): Self = StObject.set(x, "opaque", value.asInstanceOf[js.Any])
    
    inline def setOpaqueUndefined: Self = StObject.set(x, "opaque", js.undefined)
    
    inline def setQuery(value: Record[String, Any]): Self = StObject.set(x, "query", value.asInstanceOf[js.Any])
    
    inline def setQueryUndefined: Self = StObject.set(x, "query", js.undefined)
    
    inline def setRedirectionLimitReached(value: Boolean): Self = StObject.set(x, "redirectionLimitReached", value.asInstanceOf[js.Any])
    
    inline def setRedirectionLimitReachedUndefined: Self = StObject.set(x, "redirectionLimitReached", js.undefined)
    
    inline def setReset(value: Boolean): Self = StObject.set(x, "reset", value.asInstanceOf[js.Any])
    
    inline def setResetUndefined: Self = StObject.set(x, "reset", js.undefined)
    
    inline def setResponseHeader(value: raw): Self = StObject.set(x, "responseHeader", value.asInstanceOf[js.Any])
    
    inline def setResponseHeaderNull: Self = StObject.set(x, "responseHeader", null)
    
    inline def setResponseHeaderUndefined: Self = StObject.set(x, "responseHeader", js.undefined)
    
    inline def setSignal(
      value: AbortSignal | (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify EventEmitter */ Any)
    ): Self = StObject.set(x, "signal", value.asInstanceOf[js.Any])
    
    inline def setSignalNull: Self = StObject.set(x, "signal", null)
    
    inline def setSignalUndefined: Self = StObject.set(x, "signal", js.undefined)
    
    inline def setThrowOnError(value: Boolean): Self = StObject.set(x, "throwOnError", value.asInstanceOf[js.Any])
    
    inline def setThrowOnErrorUndefined: Self = StObject.set(x, "throwOnError", js.undefined)
    
    inline def setUpgrade(value: Boolean | String): Self = StObject.set(x, "upgrade", value.asInstanceOf[js.Any])
    
    inline def setUpgradeNull: Self = StObject.set(x, "upgrade", null)
    
    inline def setUpgradeUndefined: Self = StObject.set(x, "upgrade", js.undefined)
  }
}
