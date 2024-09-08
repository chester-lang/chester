package typings.undiciTypes

import typings.std.Record
import typings.undiciTypes.anon.Data
import typings.undiciTypes.dispatcherMod.default
import typings.undiciTypes.fetchMod.BodyInit
import typings.undiciTypes.fetchMod.Headers
import typings.undiciTypes.formdataMod.FormData
import typings.undiciTypes.headerMod.IncomingHttpHeaders
import typings.undiciTypes.mockInterceptorMod.MockInterceptor.MockDispatch
import typings.undiciTypes.mockInterceptorMod.MockInterceptor.MockReplyOptionsCallback
import typings.undiciTypes.mockInterceptorMod.MockInterceptor.MockResponseDataHandler
import typings.undiciTypes.mockInterceptorMod.MockInterceptor.MockResponseOptions
import typings.undiciTypes.mockInterceptorMod.MockInterceptor.Options
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

object mockInterceptorMod {
  
  /** The interceptor for a Mock. */
  @JSImport("undici-types/mock-interceptor", "MockInterceptor")
  @js.native
  open class MockInterceptor protected () extends StObject {
    def this(options: Options, mockDispatches: js.Array[MockDispatch[js.Object, js.Error]]) = this()
    
    /** Set default reply headers on the interceptor for subsequent mocked replies. */
    def defaultReplyHeaders(headers: IncomingHttpHeaders): MockInterceptor = js.native
    
    /** Set default reply trailers on the interceptor for subsequent mocked replies. */
    def defaultReplyTrailers(trailers: Record[String, String]): MockInterceptor = js.native
    
    /** Mock an undici request with the defined reply. */
    def reply[TData /* <: js.Object */](replyOptionsCallback: MockReplyOptionsCallback[TData]): MockScope[TData] = js.native
    def reply[TData /* <: js.Object */](statusCode: Double): MockScope[TData] = js.native
    def reply[TData /* <: js.Object */](
      statusCode: Double,
      data: (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify Buffer */ Any) | TData
    ): MockScope[TData] = js.native
    def reply[TData /* <: js.Object */](
      statusCode: Double,
      data: (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify Buffer */ Any) | TData,
      responseOptions: MockResponseOptions
    ): MockScope[TData] = js.native
    def reply[TData /* <: js.Object */](statusCode: Double, data: String): MockScope[TData] = js.native
    def reply[TData /* <: js.Object */](statusCode: Double, data: String, responseOptions: MockResponseOptions): MockScope[TData] = js.native
    def reply[TData /* <: js.Object */](statusCode: Double, data: Unit, responseOptions: MockResponseOptions): MockScope[TData] = js.native
    def reply[TData /* <: js.Object */](statusCode: Double, data: MockResponseDataHandler[TData]): MockScope[TData] = js.native
    def reply[TData /* <: js.Object */](statusCode: Double, data: MockResponseDataHandler[TData], responseOptions: MockResponseOptions): MockScope[TData] = js.native
    
    /** Set automatically calculated content-length header on subsequent mocked replies. */
    def replyContentLength(): MockInterceptor = js.native
    
    /** Mock an undici request by throwing the defined reply error. */
    def replyWithError[TError /* <: js.Error */](error: TError): MockScope[js.Object] = js.native
  }
  object MockInterceptor {
    
    trait MockDispatch[TData /* <: js.Object */, TError /* <: js.Error */]
      extends StObject
         with Options {
      
      var consumed: Boolean
      
      var data: MockDispatchData[TData, TError]
      
      var persist: Boolean
      
      var times: Double | Null
    }
    object MockDispatch {
      
      inline def apply[TData /* <: js.Object */, TError /* <: js.Error */](
        consumed: Boolean,
        data: MockDispatchData[TData, TError],
        path: String | js.RegExp | (js.Function1[/* path */ String, Boolean]),
        persist: Boolean
      ): MockDispatch[TData, TError] = {
        val __obj = js.Dynamic.literal(consumed = consumed.asInstanceOf[js.Any], data = data.asInstanceOf[js.Any], path = path.asInstanceOf[js.Any], persist = persist.asInstanceOf[js.Any], times = null)
        __obj.asInstanceOf[MockDispatch[TData, TError]]
      }
      
      @scala.inline
      implicit open class MutableBuilder[Self <: MockDispatch[?, ?], TData /* <: js.Object */, TError /* <: js.Error */] (val x: Self & (MockDispatch[TData, TError])) extends AnyVal {
        
        inline def setConsumed(value: Boolean): Self = StObject.set(x, "consumed", value.asInstanceOf[js.Any])
        
        inline def setData(value: MockDispatchData[TData, TError]): Self = StObject.set(x, "data", value.asInstanceOf[js.Any])
        
        inline def setPersist(value: Boolean): Self = StObject.set(x, "persist", value.asInstanceOf[js.Any])
        
        inline def setTimes(value: Double): Self = StObject.set(x, "times", value.asInstanceOf[js.Any])
        
        inline def setTimesNull: Self = StObject.set(x, "times", null)
      }
    }
    
    trait MockDispatchData[TData /* <: js.Object */, TError /* <: js.Error */]
      extends StObject
         with MockResponseOptions {
      
      var data: js.UndefOr[TData | String] = js.undefined
      
      var error: TError | Null
      
      var statusCode: js.UndefOr[Double] = js.undefined
    }
    object MockDispatchData {
      
      inline def apply[TData /* <: js.Object */, TError /* <: js.Error */](): MockDispatchData[TData, TError] = {
        val __obj = js.Dynamic.literal(error = null)
        __obj.asInstanceOf[MockDispatchData[TData, TError]]
      }
      
      @scala.inline
      implicit open class MutableBuilder[Self <: MockDispatchData[?, ?], TData /* <: js.Object */, TError /* <: js.Error */] (val x: Self & (MockDispatchData[TData, TError])) extends AnyVal {
        
        inline def setData(value: TData | String): Self = StObject.set(x, "data", value.asInstanceOf[js.Any])
        
        inline def setDataUndefined: Self = StObject.set(x, "data", js.undefined)
        
        inline def setError(value: TError): Self = StObject.set(x, "error", value.asInstanceOf[js.Any])
        
        inline def setErrorNull: Self = StObject.set(x, "error", null)
        
        inline def setStatusCode(value: Double): Self = StObject.set(x, "statusCode", value.asInstanceOf[js.Any])
        
        inline def setStatusCodeUndefined: Self = StObject.set(x, "statusCode", js.undefined)
      }
    }
    
    type MockReplyOptionsCallback[TData /* <: js.Object */] = js.Function1[/* opts */ MockResponseCallbackOptions, Data[TData]]
    
    trait MockResponseCallbackOptions extends StObject {
      
      var body: js.UndefOr[
            BodyInit | String | (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify Buffer */ Any) | js.typedarray.Uint8Array | Null | FormData
          ] = js.undefined
      
      var headers: js.UndefOr[Headers | (Record[String, String])] = js.undefined
      
      var maxRedirections: js.UndefOr[Double] = js.undefined
      
      var method: String
      
      var origin: js.UndefOr[String] = js.undefined
      
      var path: String
    }
    object MockResponseCallbackOptions {
      
      inline def apply(method: String, path: String): MockResponseCallbackOptions = {
        val __obj = js.Dynamic.literal(method = method.asInstanceOf[js.Any], path = path.asInstanceOf[js.Any])
        __obj.asInstanceOf[MockResponseCallbackOptions]
      }
      
      @scala.inline
      implicit open class MutableBuilder[Self <: MockResponseCallbackOptions] (val x: Self) extends AnyVal {
        
        inline def setBody(
          value: BodyInit | String | (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify Buffer */ Any) | js.typedarray.Uint8Array | FormData
        ): Self = StObject.set(x, "body", value.asInstanceOf[js.Any])
        
        inline def setBodyNull: Self = StObject.set(x, "body", null)
        
        inline def setBodyUndefined: Self = StObject.set(x, "body", js.undefined)
        
        inline def setHeaders(value: Headers | (Record[String, String])): Self = StObject.set(x, "headers", value.asInstanceOf[js.Any])
        
        inline def setHeadersUndefined: Self = StObject.set(x, "headers", js.undefined)
        
        inline def setMaxRedirections(value: Double): Self = StObject.set(x, "maxRedirections", value.asInstanceOf[js.Any])
        
        inline def setMaxRedirectionsUndefined: Self = StObject.set(x, "maxRedirections", js.undefined)
        
        inline def setMethod(value: String): Self = StObject.set(x, "method", value.asInstanceOf[js.Any])
        
        inline def setOrigin(value: String): Self = StObject.set(x, "origin", value.asInstanceOf[js.Any])
        
        inline def setOriginUndefined: Self = StObject.set(x, "origin", js.undefined)
        
        inline def setPath(value: String): Self = StObject.set(x, "path", value.asInstanceOf[js.Any])
      }
    }
    
    type MockResponseDataHandler[TData /* <: js.Object */] = js.Function1[
        /* opts */ MockResponseCallbackOptions, 
        TData | (/* import warning: transforms.QualifyReferences#resolveTypeRef many Couldn't qualify Buffer */ Any) | String
      ]
    
    trait MockResponseOptions extends StObject {
      
      var headers: js.UndefOr[IncomingHttpHeaders] = js.undefined
      
      var trailers: js.UndefOr[Record[String, String]] = js.undefined
    }
    object MockResponseOptions {
      
      inline def apply(): MockResponseOptions = {
        val __obj = js.Dynamic.literal()
        __obj.asInstanceOf[MockResponseOptions]
      }
      
      @scala.inline
      implicit open class MutableBuilder[Self <: MockResponseOptions] (val x: Self) extends AnyVal {
        
        inline def setHeaders(value: IncomingHttpHeaders): Self = StObject.set(x, "headers", value.asInstanceOf[js.Any])
        
        inline def setHeadersUndefined: Self = StObject.set(x, "headers", js.undefined)
        
        inline def setTrailers(value: Record[String, String]): Self = StObject.set(x, "trailers", value.asInstanceOf[js.Any])
        
        inline def setTrailersUndefined: Self = StObject.set(x, "trailers", js.undefined)
      }
    }
    
    trait Options extends StObject {
      
      /** Body to intercept on. */
      var body: js.UndefOr[String | js.RegExp | (js.Function1[/* body */ String, Boolean])] = js.undefined
      
      /** Headers to intercept on. */
      var headers: js.UndefOr[
            (Record[String, String | js.RegExp | (js.Function1[/* body */ String, Boolean])]) | (js.Function1[/* headers */ Record[String, String], Boolean])
          ] = js.undefined
      
      /** Method to intercept on. Defaults to GET. */
      var method: js.UndefOr[String | js.RegExp | (js.Function1[/* method */ String, Boolean])] = js.undefined
      
      /** Path to intercept on. */
      var path: String | js.RegExp | (js.Function1[/* path */ String, Boolean])
      
      /** Query params to intercept on */
      var query: js.UndefOr[Record[String, Any]] = js.undefined
    }
    object Options {
      
      inline def apply(path: String | js.RegExp | (js.Function1[/* path */ String, Boolean])): Options = {
        val __obj = js.Dynamic.literal(path = path.asInstanceOf[js.Any])
        __obj.asInstanceOf[Options]
      }
      
      @scala.inline
      implicit open class MutableBuilder[Self <: Options] (val x: Self) extends AnyVal {
        
        inline def setBody(value: String | js.RegExp | (js.Function1[/* body */ String, Boolean])): Self = StObject.set(x, "body", value.asInstanceOf[js.Any])
        
        inline def setBodyFunction1(value: /* body */ String => Boolean): Self = StObject.set(x, "body", js.Any.fromFunction1(value))
        
        inline def setBodyUndefined: Self = StObject.set(x, "body", js.undefined)
        
        inline def setHeaders(
          value: (Record[String, String | js.RegExp | (js.Function1[/* body */ String, Boolean])]) | (js.Function1[/* headers */ Record[String, String], Boolean])
        ): Self = StObject.set(x, "headers", value.asInstanceOf[js.Any])
        
        inline def setHeadersFunction1(value: /* headers */ Record[String, String] => Boolean): Self = StObject.set(x, "headers", js.Any.fromFunction1(value))
        
        inline def setHeadersUndefined: Self = StObject.set(x, "headers", js.undefined)
        
        inline def setMethod(value: String | js.RegExp | (js.Function1[/* method */ String, Boolean])): Self = StObject.set(x, "method", value.asInstanceOf[js.Any])
        
        inline def setMethodFunction1(value: /* method */ String => Boolean): Self = StObject.set(x, "method", js.Any.fromFunction1(value))
        
        inline def setMethodUndefined: Self = StObject.set(x, "method", js.undefined)
        
        inline def setPath(value: String | js.RegExp | (js.Function1[/* path */ String, Boolean])): Self = StObject.set(x, "path", value.asInstanceOf[js.Any])
        
        inline def setPathFunction1(value: /* path */ String => Boolean): Self = StObject.set(x, "path", js.Any.fromFunction1(value))
        
        inline def setQuery(value: Record[String, Any]): Self = StObject.set(x, "query", value.asInstanceOf[js.Any])
        
        inline def setQueryUndefined: Self = StObject.set(x, "query", js.undefined)
      }
    }
  }
  
  /** The scope associated with a mock dispatch. */
  @JSImport("undici-types/mock-interceptor", "MockScope")
  @js.native
  open class MockScope[TData /* <: js.Object */] protected () extends StObject {
    def this(mockDispatch: MockDispatch[TData, js.Error]) = this()
    
    /** Delay a reply by a set amount of time in ms. */
    def delay(waitInMs: Double): MockScope[TData] = js.native
    
    /** Persist the defined mock data for the associated reply. It will return the defined mock data indefinitely. */
    def persist(): MockScope[TData] = js.native
    
    /** Define a reply for a set amount of matching requests. */
    def times(repeatTimes: Double): MockScope[TData] = js.native
  }
  
  @js.native
  trait Interceptable extends default {
    
    /** Intercepts any matching requests that use the same origin as this mock client. */
    def intercept(options: Options): MockInterceptor = js.native
  }
}