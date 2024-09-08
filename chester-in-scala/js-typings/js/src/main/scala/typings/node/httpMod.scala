package typings.node

import org.scalablytyped.runtime.Instantiable1
import typings.node.anon.Req
import typings.node.bufferMod.global.Buffer
import typings.node.dnsMod.LookupAddress
import typings.node.dnsMod.LookupOptions
import typings.node.globalsMod.global.AbortSignal
import typings.node.globalsMod.global.NodeJS.Dict
import typings.node.globalsMod.global.NodeJS.ErrnoException
import typings.node.globalsMod.global.NodeJS.ReadOnlyDict
import typings.node.netMod.LookupFunction
import typings.node.netMod.OnReadOpts
import typings.node.nodeColonnetMod.Socket
import typings.node.nodeColonstreamMod.Duplex
import typings.node.nodeColonstreamMod.Readable
import typings.node.nodeColonurlMod.URL
import typings.node.nodeStrings.abort
import typings.node.nodeStrings.checkContinue
import typings.node.nodeStrings.checkExpectation
import typings.node.nodeStrings.clientError
import typings.node.nodeStrings.close
import typings.node.nodeStrings.connect
import typings.node.nodeStrings.connection
import typings.node.nodeStrings.continue
import typings.node.nodeStrings.drain
import typings.node.nodeStrings.dropRequest
import typings.node.nodeStrings.error
import typings.node.nodeStrings.fifo
import typings.node.nodeStrings.finish
import typings.node.nodeStrings.information
import typings.node.nodeStrings.lifo
import typings.node.nodeStrings.listening
import typings.node.nodeStrings.pipe
import typings.node.nodeStrings.request
import typings.node.nodeStrings.response
import typings.node.nodeStrings.socket
import typings.node.nodeStrings.timeout
import typings.node.nodeStrings.unpipe
import typings.node.nodeStrings.upgrade
import typings.std.InstanceType
import typings.std.Record
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

object httpMod {
  
  @JSImport("http", JSImport.Namespace)
  @js.native
  val ^ : js.Any = js.native
  
  /**
    * An `Agent` is responsible for managing connection persistence
    * and reuse for HTTP clients. It maintains a queue of pending requests
    * for a given host and port, reusing a single socket connection for each
    * until the queue is empty, at which time the socket is either destroyed
    * or put into a pool where it is kept to be used again for requests to the
    * same host and port. Whether it is destroyed or pooled depends on the `keepAlive` `option`.
    *
    * Pooled connections have TCP Keep-Alive enabled for them, but servers may
    * still close idle connections, in which case they will be removed from the
    * pool and a new connection will be made when a new HTTP request is made for
    * that host and port. Servers may also refuse to allow multiple requests
    * over the same connection, in which case the connection will have to be
    * remade for every request and cannot be pooled. The `Agent` will still make
    * the requests to that server, but each one will occur over a new connection.
    *
    * When a connection is closed by the client or the server, it is removed
    * from the pool. Any unused sockets in the pool will be unrefed so as not
    * to keep the Node.js process running when there are no outstanding requests.
    * (see `socket.unref()`).
    *
    * It is good practice, to `destroy()` an `Agent` instance when it is no
    * longer in use, because unused sockets consume OS resources.
    *
    * Sockets are removed from an agent when the socket emits either
    * a `'close'` event or an `'agentRemove'` event. When intending to keep one
    * HTTP request open for a long time without keeping it in the agent, something
    * like the following may be done:
    *
    * ```js
    * http.get(options, (res) => {
    *   // Do stuff
    * }).on('socket', (socket) => {
    *   socket.emit('agentRemove');
    * });
    * ```
    *
    * An agent may also be used for an individual request. By providing `{agent: false}` as an option to the `http.get()` or `http.request()` functions, a one-time use `Agent` with default options
    * will be used
    * for the client connection.
    *
    * `agent:false`:
    *
    * ```js
    * http.get({
    *   hostname: 'localhost',
    *   port: 80,
    *   path: '/',
    *   agent: false,  // Create a new agent just for this one request
    * }, (res) => {
    *   // Do stuff with response
    * });
    * ```
    *
    * `options` in [`socket.connect()`](https://nodejs.org/docs/latest-v22.x/api/net.html#socketconnectoptions-connectlistener) are also supported.
    *
    * To configure any of them, a custom {@link Agent} instance must be created.
    *
    * ```js
    * const http = require('node:http');
    * const keepAliveAgent = new http.Agent({ keepAlive: true });
    * options.agent = keepAliveAgent;
    * http.request(options, onResponseCallback)
    * ```
    * @since v0.3.4
    */
  @JSImport("http", "Agent")
  @js.native
  open class Agent () extends StObject {
    def this(opts: AgentOptions) = this()
    
    /**
      * Destroy any sockets that are currently in use by the agent.
      *
      * It is usually not necessary to do this. However, if using an
      * agent with `keepAlive` enabled, then it is best to explicitly shut down
      * the agent when it is no longer needed. Otherwise,
      * sockets might stay open for quite a long time before the server
      * terminates them.
      * @since v0.11.4
      */
    def destroy(): Unit = js.native
    
    /**
      * An object which contains arrays of sockets currently awaiting use by
      * the agent when `keepAlive` is enabled. Do not modify.
      *
      * Sockets in the `freeSockets` list will be automatically destroyed and
      * removed from the array on `'timeout'`.
      * @since v0.11.4
      */
    val freeSockets: ReadOnlyDict[js.Array[Socket]] = js.native
    
    /**
      * By default set to 256. For agents with `keepAlive` enabled, this
      * sets the maximum number of sockets that will be left open in the free
      * state.
      * @since v0.11.7
      */
    var maxFreeSockets: Double = js.native
    
    /**
      * By default set to `Infinity`. Determines how many concurrent sockets the agent
      * can have open per origin. Origin is the returned value of `agent.getName()`.
      * @since v0.3.6
      */
    var maxSockets: Double = js.native
    
    /**
      * By default set to `Infinity`. Determines how many concurrent sockets the agent
      * can have open. Unlike `maxSockets`, this parameter applies across all origins.
      * @since v14.5.0, v12.19.0
      */
    var maxTotalSockets: Double = js.native
    
    /**
      * An object which contains queues of requests that have not yet been assigned to
      * sockets. Do not modify.
      * @since v0.5.9
      */
    val requests: ReadOnlyDict[js.Array[IncomingMessage]] = js.native
    
    /**
      * An object which contains arrays of sockets currently in use by the
      * agent. Do not modify.
      * @since v0.3.6
      */
    val sockets: ReadOnlyDict[js.Array[Socket]] = js.native
  }
  
  /**
    * This object is created internally and returned from {@link request}. It
    * represents an _in-progress_ request whose header has already been queued. The
    * header is still mutable using the `setHeader(name, value)`, `getHeader(name)`, `removeHeader(name)` API. The actual header will
    * be sent along with the first data chunk or when calling `request.end()`.
    *
    * To get the response, add a listener for `'response'` to the request object. `'response'` will be emitted from the request object when the response
    * headers have been received. The `'response'` event is executed with one
    * argument which is an instance of {@link IncomingMessage}.
    *
    * During the `'response'` event, one can add listeners to the
    * response object; particularly to listen for the `'data'` event.
    *
    * If no `'response'` handler is added, then the response will be
    * entirely discarded. However, if a `'response'` event handler is added,
    * then the data from the response object **must** be consumed, either by
    * calling `response.read()` whenever there is a `'readable'` event, or
    * by adding a `'data'` handler, or by calling the `.resume()` method.
    * Until the data is consumed, the `'end'` event will not fire. Also, until
    * the data is read it will consume memory that can eventually lead to a
    * 'process out of memory' error.
    *
    * For backward compatibility, `res` will only emit `'error'` if there is an `'error'` listener registered.
    *
    * Set `Content-Length` header to limit the response body size.
    * If `response.strictContentLength` is set to `true`, mismatching the `Content-Length` header value will result in an `Error` being thrown,
    * identified by `code:``'ERR_HTTP_CONTENT_LENGTH_MISMATCH'`.
    *
    * `Content-Length` value should be in bytes, not characters. Use `Buffer.byteLength()` to determine the length of the body in bytes.
    * @since v0.1.17
    */
  @JSImport("http", "ClientRequest")
  @js.native
  open class ClientRequest protected () extends StObject {
    def this(url: String) = this()
    def this(url: ClientRequestArgs) = this()
    def this(url: URL) = this()
    def this(url: String, cb: js.Function1[/* res */ IncomingMessage, Unit]) = this()
    def this(url: ClientRequestArgs, cb: js.Function1[/* res */ IncomingMessage, Unit]) = this()
    def this(url: URL, cb: js.Function1[/* res */ IncomingMessage, Unit]) = this()
    
    /**
      * Marks the request as aborting. Calling this will cause remaining data
      * in the response to be dropped and the socket to be destroyed.
      * @since v0.3.8
      * @deprecated Since v14.1.0,v13.14.0 - Use `destroy` instead.
      */
    def abort(): Unit = js.native
    
    /**
      * The `request.aborted` property will be `true` if the request has
      * been aborted.
      * @since v0.11.14
      * @deprecated Since v17.0.0, v16.12.0 - Check `destroyed` instead.
      */
    var aborted: Boolean = js.native
    
    def addListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    def addListener(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    /**
      * @deprecated
      */
    @JSName("addListener")
    def addListener_abort(event: abort, listener: js.Function0[Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_connect(
      event: connect,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("addListener")
    def addListener_continue(event: continue, listener: js.Function0[Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_information(event: information, listener: js.Function1[/* info */ InformationEvent, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_response(event: response, listener: js.Function1[/* response */ IncomingMessage, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_socket(event: socket, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_timeout(event: timeout, listener: js.Function0[Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_upgrade(
      event: upgrade,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    /**
      * Returns an array containing the unique names of the current outgoing raw
      * headers. Header names are returned with their exact casing being set.
      *
      * ```js
      * request.setHeader('Foo', 'bar');
      * request.setHeader('Set-Cookie', ['foo=bar', 'bar=baz']);
      *
      * const headerNames = request.getRawHeaderNames();
      * // headerNames === ['Foo', 'Set-Cookie']
      * ```
      * @since v15.13.0, v14.17.0
      */
    def getRawHeaderNames(): js.Array[String] = js.native
    
    /**
      * The request host.
      * @since v14.5.0, v12.19.0
      */
    var host: String = js.native
    
    /**
      * Limits maximum response headers count. If set to 0, no limit will be applied.
      */
    var maxHeadersCount: Double = js.native
    
    /**
      * The request method.
      * @since v0.1.97
      */
    var method: String = js.native
    
    def on(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    def on(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    
    def onSocket(socket: Socket): Unit = js.native
    
    /**
      * @deprecated
      */
    @JSName("on")
    def on_abort(event: abort, listener: js.Function0[Unit]): this.type = js.native
    @JSName("on")
    def on_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("on")
    def on_connect(
      event: connect,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("on")
    def on_continue(event: continue, listener: js.Function0[Unit]): this.type = js.native
    @JSName("on")
    def on_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
    @JSName("on")
    def on_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("on")
    def on_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
    @JSName("on")
    def on_information(event: information, listener: js.Function1[/* info */ InformationEvent, Unit]): this.type = js.native
    @JSName("on")
    def on_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("on")
    def on_response(event: response, listener: js.Function1[/* response */ IncomingMessage, Unit]): this.type = js.native
    @JSName("on")
    def on_socket(event: socket, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("on")
    def on_timeout(event: timeout, listener: js.Function0[Unit]): this.type = js.native
    @JSName("on")
    def on_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("on")
    def on_upgrade(
      event: upgrade,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    def once(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    def once(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    /**
      * @deprecated
      */
    @JSName("once")
    def once_abort(event: abort, listener: js.Function0[Unit]): this.type = js.native
    @JSName("once")
    def once_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("once")
    def once_connect(
      event: connect,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("once")
    def once_continue(event: continue, listener: js.Function0[Unit]): this.type = js.native
    @JSName("once")
    def once_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
    @JSName("once")
    def once_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("once")
    def once_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
    @JSName("once")
    def once_information(event: information, listener: js.Function1[/* info */ InformationEvent, Unit]): this.type = js.native
    @JSName("once")
    def once_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("once")
    def once_response(event: response, listener: js.Function1[/* response */ IncomingMessage, Unit]): this.type = js.native
    @JSName("once")
    def once_socket(event: socket, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("once")
    def once_timeout(event: timeout, listener: js.Function0[Unit]): this.type = js.native
    @JSName("once")
    def once_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("once")
    def once_upgrade(
      event: upgrade,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    /**
      * The request path.
      * @since v0.4.0
      */
    var path: String = js.native
    
    def prependListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    def prependListener(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    /**
      * @deprecated
      */
    @JSName("prependListener")
    def prependListener_abort(event: abort, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_connect(
      event: connect,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("prependListener")
    def prependListener_continue(event: continue, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_information(event: information, listener: js.Function1[/* info */ InformationEvent, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_response(event: response, listener: js.Function1[/* response */ IncomingMessage, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_socket(event: socket, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_timeout(event: timeout, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_upgrade(
      event: upgrade,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    def prependOnceListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    def prependOnceListener(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    /**
      * @deprecated
      */
    @JSName("prependOnceListener")
    def prependOnceListener_abort(event: abort, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_connect(
      event: connect,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_continue(event: continue, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_information(event: information, listener: js.Function1[/* info */ InformationEvent, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_response(event: response, listener: js.Function1[/* response */ IncomingMessage, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_socket(event: socket, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_timeout(event: timeout, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_upgrade(
      event: upgrade,
      listener: js.Function3[/* response */ IncomingMessage, /* socket */ Socket, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    /**
      * The request protocol.
      * @since v14.5.0, v12.19.0
      */
    var protocol: String = js.native
    
    /**
      * When sending request through a keep-alive enabled agent, the underlying socket
      * might be reused. But if server closes connection at unfortunate time, client
      * may run into a 'ECONNRESET' error.
      *
      * ```js
      * import http from 'node:http';
      *
      * // Server has a 5 seconds keep-alive timeout by default
      * http
      *   .createServer((req, res) => {
      *     res.write('hello\n');
      *     res.end();
      *   })
      *   .listen(3000);
      *
      * setInterval(() => {
      *   // Adapting a keep-alive agent
      *   http.get('http://localhost:3000', { agent }, (res) => {
      *     res.on('data', (data) => {
      *       // Do nothing
      *     });
      *   });
      * }, 5000); // Sending request on 5s interval so it's easy to hit idle timeout
      * ```
      *
      * By marking a request whether it reused socket or not, we can do
      * automatic error retry base on it.
      *
      * ```js
      * import http from 'node:http';
      * const agent = new http.Agent({ keepAlive: true });
      *
      * function retriableRequest() {
      *   const req = http
      *     .get('http://localhost:3000', { agent }, (res) => {
      *       // ...
      *     })
      *     .on('error', (err) => {
      *       // Check if retry is needed
      *       if (req.reusedSocket &#x26;&#x26; err.code === 'ECONNRESET') {
      *         retriableRequest();
      *       }
      *     });
      * }
      *
      * retriableRequest();
      * ```
      * @since v13.0.0, v12.16.0
      */
    var reusedSocket: Boolean = js.native
    
    /**
      * Once a socket is assigned to this request and is connected `socket.setNoDelay()` will be called.
      * @since v0.5.9
      */
    def setNoDelay(): Unit = js.native
    def setNoDelay(noDelay: Boolean): Unit = js.native
    
    /**
      * Once a socket is assigned to this request and is connected `socket.setKeepAlive()` will be called.
      * @since v0.5.9
      */
    def setSocketKeepAlive(): Unit = js.native
    def setSocketKeepAlive(enable: Boolean): Unit = js.native
    def setSocketKeepAlive(enable: Boolean, initialDelay: Double): Unit = js.native
    def setSocketKeepAlive(enable: Unit, initialDelay: Double): Unit = js.native
    
    /**
      * Once a socket is assigned to this request and is connected `socket.setTimeout()` will be called.
      * @since v0.5.9
      * @param timeout Milliseconds before a request times out.
      * @param callback Optional function to be called when a timeout occurs. Same as binding to the `'timeout'` event.
      */
    def setTimeout(timeout: Double): this.type = js.native
    def setTimeout(timeout: Double, callback: js.Function0[Unit]): this.type = js.native
  }
  
  /**
    * An `IncomingMessage` object is created by {@link Server} or {@link ClientRequest} and passed as the first argument to the `'request'` and `'response'` event respectively. It may be used to
    * access response
    * status, headers, and data.
    *
    * Different from its `socket` value which is a subclass of `stream.Duplex`, the `IncomingMessage` itself extends `stream.Readable` and is created separately to
    * parse and emit the incoming HTTP headers and payload, as the underlying socket
    * may be reused multiple times in case of keep-alive.
    * @since v0.1.17
    */
  @JSImport("http", "IncomingMessage")
  @js.native
  open class IncomingMessage protected () extends StObject {
    def this(socket: Socket) = this()
    
    /**
      * The `message.aborted` property will be `true` if the request has
      * been aborted.
      * @since v10.1.0
      * @deprecated Since v17.0.0,v16.12.0 - Check `message.destroyed` from <a href="stream.html#class-streamreadable" class="type">stream.Readable</a>.
      */
    var aborted: Boolean = js.native
    
    /**
      * The `message.complete` property will be `true` if a complete HTTP message has
      * been received and successfully parsed.
      *
      * This property is particularly useful as a means of determining if a client or
      * server fully transmitted a message before a connection was terminated:
      *
      * ```js
      * const req = http.request({
      *   host: '127.0.0.1',
      *   port: 8080,
      *   method: 'POST',
      * }, (res) => {
      *   res.resume();
      *   res.on('end', () => {
      *     if (!res.complete)
      *       console.error(
      *         'The connection was terminated while the message was still being sent');
      *   });
      * });
      * ```
      * @since v0.3.0
      */
    var complete: Boolean = js.native
    
    /**
      * Alias for `message.socket`.
      * @since v0.1.90
      * @deprecated Since v16.0.0 - Use `socket`.
      */
    var connection: Socket = js.native
    
    /**
      * Calls `destroy()` on the socket that received the `IncomingMessage`. If `error` is provided, an `'error'` event is emitted on the socket and `error` is passed
      * as an argument to any listeners on the event.
      * @since v0.3.0
      */
    def destroy(): this.type = js.native
    def destroy(error: js.Error): this.type = js.native
    
    /**
      * The request/response headers object.
      *
      * Key-value pairs of header names and values. Header names are lower-cased.
      *
      * ```js
      * // Prints something like:
      * //
      * // { 'user-agent': 'curl/7.22.0',
      * //   host: '127.0.0.1:8000',
      * //   accept: '*' }
      * console.log(request.headers);
      * ```
      *
      * Duplicates in raw headers are handled in the following ways, depending on the
      * header name:
      *
      * * Duplicates of `age`, `authorization`, `content-length`, `content-type`, `etag`, `expires`, `from`, `host`, `if-modified-since`, `if-unmodified-since`, `last-modified`, `location`,
      * `max-forwards`, `proxy-authorization`, `referer`, `retry-after`, `server`, or `user-agent` are discarded.
      * To allow duplicate values of the headers listed above to be joined,
      * use the option `joinDuplicateHeaders` in {@link request} and {@link createServer}. See RFC 9110 Section 5.3 for more
      * information.
      * * `set-cookie` is always an array. Duplicates are added to the array.
      * * For duplicate `cookie` headers, the values are joined together with `; `.
      * * For all other headers, the values are joined together with `, `.
      * @since v0.1.5
      */
    var headers: IncomingHttpHeaders = js.native
    
    /**
      * Similar to `message.headers`, but there is no join logic and the values are
      * always arrays of strings, even for headers received just once.
      *
      * ```js
      * // Prints something like:
      * //
      * // { 'user-agent': ['curl/7.22.0'],
      * //   host: ['127.0.0.1:8000'],
      * //   accept: ['*'] }
      * console.log(request.headersDistinct);
      * ```
      * @since v18.3.0, v16.17.0
      */
    var headersDistinct: Dict[js.Array[String]] = js.native
    
    /**
      * In case of server request, the HTTP version sent by the client. In the case of
      * client response, the HTTP version of the connected-to server.
      * Probably either `'1.1'` or `'1.0'`.
      *
      * Also `message.httpVersionMajor` is the first integer and `message.httpVersionMinor` is the second.
      * @since v0.1.1
      */
    var httpVersion: String = js.native
    
    var httpVersionMajor: Double = js.native
    
    var httpVersionMinor: Double = js.native
    
    /**
      * **Only valid for request obtained from {@link Server}.**
      *
      * The request method as a string. Read only. Examples: `'GET'`, `'DELETE'`.
      * @since v0.1.1
      */
    var method: js.UndefOr[String] = js.native
    
    /**
      * The raw request/response headers list exactly as they were received.
      *
      * The keys and values are in the same list. It is _not_ a
      * list of tuples. So, the even-numbered offsets are key values, and the
      * odd-numbered offsets are the associated values.
      *
      * Header names are not lowercased, and duplicates are not merged.
      *
      * ```js
      * // Prints something like:
      * //
      * // [ 'user-agent',
      * //   'this is invalid because there can be only one',
      * //   'User-Agent',
      * //   'curl/7.22.0',
      * //   'Host',
      * //   '127.0.0.1:8000',
      * //   'ACCEPT',
      * //   '*' ]
      * console.log(request.rawHeaders);
      * ```
      * @since v0.11.6
      */
    var rawHeaders: js.Array[String] = js.native
    
    /**
      * The raw request/response trailer keys and values exactly as they were
      * received. Only populated at the `'end'` event.
      * @since v0.11.6
      */
    var rawTrailers: js.Array[String] = js.native
    
    /**
      * Calls `message.socket.setTimeout(msecs, callback)`.
      * @since v0.5.9
      */
    def setTimeout(msecs: Double): this.type = js.native
    def setTimeout(msecs: Double, callback: js.Function0[Unit]): this.type = js.native
    
    /**
      * The `net.Socket` object associated with the connection.
      *
      * With HTTPS support, use `request.socket.getPeerCertificate()` to obtain the
      * client's authentication details.
      *
      * This property is guaranteed to be an instance of the `net.Socket` class,
      * a subclass of `stream.Duplex`, unless the user specified a socket
      * type other than `net.Socket` or internally nulled.
      * @since v0.3.0
      */
    var socket: Socket = js.native
    
    /**
      * **Only valid for response obtained from {@link ClientRequest}.**
      *
      * The 3-digit HTTP response status code. E.G. `404`.
      * @since v0.1.1
      */
    var statusCode: js.UndefOr[Double] = js.native
    
    /**
      * **Only valid for response obtained from {@link ClientRequest}.**
      *
      * The HTTP response status message (reason phrase). E.G. `OK` or `Internal Server Error`.
      * @since v0.11.10
      */
    var statusMessage: js.UndefOr[String] = js.native
    
    /**
      * The request/response trailers object. Only populated at the `'end'` event.
      * @since v0.3.0
      */
    var trailers: Dict[String] = js.native
    
    /**
      * Similar to `message.trailers`, but there is no join logic and the values are
      * always arrays of strings, even for headers received just once.
      * Only populated at the `'end'` event.
      * @since v18.3.0, v16.17.0
      */
    var trailersDistinct: Dict[js.Array[String]] = js.native
    
    /**
      * **Only valid for request obtained from {@link Server}.**
      *
      * Request URL string. This contains only the URL that is present in the actual
      * HTTP request. Take the following request:
      *
      * ```http
      * GET /status?name=ryan HTTP/1.1
      * Accept: text/plain
      * ```
      *
      * To parse the URL into its parts:
      *
      * ```js
      * new URL(`http://${process.env.HOST ?? 'localhost'}${request.url}`);
      * ```
      *
      * When `request.url` is `'/status?name=ryan'` and `process.env.HOST` is undefined:
      *
      * ```console
      * $ node
      * > new URL(`http://${process.env.HOST ?? 'localhost'}${request.url}`);
      * URL {
      *   href: 'http://localhost/status?name=ryan',
      *   origin: 'http://localhost',
      *   protocol: 'http:',
      *   username: '',
      *   password: '',
      *   host: 'localhost',
      *   hostname: 'localhost',
      *   port: '',
      *   pathname: '/status',
      *   search: '?name=ryan',
      *   searchParams: URLSearchParams { 'name' => 'ryan' },
      *   hash: ''
      * }
      * ```
      *
      * Ensure that you set `process.env.HOST` to the server's host name, or consider replacing this part entirely. If using `req.headers.host`, ensure proper
      * validation is used, as clients may specify a custom `Host` header.
      * @since v0.1.90
      */
    var url: js.UndefOr[String] = js.native
  }
  
  @JSImport("http", "METHODS")
  @js.native
  val METHODS: js.Array[String] = js.native
  
  /**
    * This class serves as the parent class of {@link ClientRequest} and {@link ServerResponse}. It is an abstract outgoing message from
    * the perspective of the participants of an HTTP transaction.
    * @since v0.1.17
    */
  @JSImport("http", "OutgoingMessage")
  @js.native
  open class OutgoingMessage[Request /* <: IncomingMessage */] () extends StObject {
    
    def addTrailers(headers: js.Array[js.Tuple2[String, String]]): Unit = js.native
    /**
      * Adds HTTP trailers (headers but at the end of the message) to the message.
      *
      * Trailers will **only** be emitted if the message is chunked encoded. If not,
      * the trailers will be silently discarded.
      *
      * HTTP requires the `Trailer` header to be sent to emit trailers,
      * with a list of header field names in its value, e.g.
      *
      * ```js
      * message.writeHead(200, { 'Content-Type': 'text/plain',
      *                          'Trailer': 'Content-MD5' });
      * message.write(fileData);
      * message.addTrailers({ 'Content-MD5': '7895bf4b8828b55ceaf47747b4bca667' });
      * message.end();
      * ```
      *
      * Attempting to set a header field name or value that contains invalid characters
      * will result in a `TypeError` being thrown.
      * @since v0.3.0
      */
    def addTrailers(headers: OutgoingHttpHeaders): Unit = js.native
    
    /**
      * Append a single header value to the header object.
      *
      * If the value is an array, this is equivalent to calling this method multiple
      * times.
      *
      * If there were no previous values for the header, this is equivalent to calling `outgoingMessage.setHeader(name, value)`.
      *
      * Depending of the value of `options.uniqueHeaders` when the client request or the
      * server were created, this will end up in the header being sent multiple times or
      * a single time with values joined using `; `.
      * @since v18.3.0, v16.17.0
      * @param name Header name
      * @param value Header value
      */
    def appendHeader(name: String, value: String): this.type = js.native
    def appendHeader(name: String, value: js.Array[String]): this.type = js.native
    
    var chunkedEncoding: Boolean = js.native
    
    /**
      * Alias of `outgoingMessage.socket`.
      * @since v0.3.0
      * @deprecated Since v15.12.0,v14.17.1 - Use `socket` instead.
      */
    val connection: Socket | Null = js.native
    
    /**
      * @deprecated Use `writableEnded` instead.
      */
    var finished: Boolean = js.native
    
    /**
      * Flushes the message headers.
      *
      * For efficiency reason, Node.js normally buffers the message headers
      * until `outgoingMessage.end()` is called or the first chunk of message data
      * is written. It then tries to pack the headers and data into a single TCP
      * packet.
      *
      * It is usually desired (it saves a TCP round-trip), but not when the first
      * data is not sent until possibly much later. `outgoingMessage.flushHeaders()` bypasses the optimization and kickstarts the message.
      * @since v1.6.0
      */
    def flushHeaders(): Unit = js.native
    
    /**
      * Gets the value of the HTTP header with the given name. If that header is not
      * set, the returned value will be `undefined`.
      * @since v0.4.0
      * @param name Name of header
      */
    def getHeader(name: String): js.UndefOr[Double | String | js.Array[String]] = js.native
    
    /**
      * Returns an array containing the unique names of the current outgoing headers.
      * All names are lowercase.
      * @since v7.7.0
      */
    def getHeaderNames(): js.Array[String] = js.native
    
    /**
      * Returns a shallow copy of the current outgoing headers. Since a shallow
      * copy is used, array values may be mutated without additional calls to
      * various header-related HTTP module methods. The keys of the returned
      * object are the header names and the values are the respective header
      * values. All header names are lowercase.
      *
      * The object returned by the `outgoingMessage.getHeaders()` method does
      * not prototypically inherit from the JavaScript `Object`. This means that
      * typical `Object` methods such as `obj.toString()`, `obj.hasOwnProperty()`,
      * and others are not defined and will not work.
      *
      * ```js
      * outgoingMessage.setHeader('Foo', 'bar');
      * outgoingMessage.setHeader('Set-Cookie', ['foo=bar', 'bar=baz']);
      *
      * const headers = outgoingMessage.getHeaders();
      * // headers === { foo: 'bar', 'set-cookie': ['foo=bar', 'bar=baz'] }
      * ```
      * @since v7.7.0
      */
    def getHeaders(): OutgoingHttpHeaders = js.native
    
    /**
      * Returns `true` if the header identified by `name` is currently set in the
      * outgoing headers. The header name is case-insensitive.
      *
      * ```js
      * const hasContentType = outgoingMessage.hasHeader('content-type');
      * ```
      * @since v7.7.0
      */
    def hasHeader(name: String): Boolean = js.native
    
    /**
      * Read-only. `true` if the headers were sent, otherwise `false`.
      * @since v0.9.3
      */
    val headersSent: Boolean = js.native
    
    /**
      * Removes a header that is queued for implicit sending.
      *
      * ```js
      * outgoingMessage.removeHeader('Content-Encoding');
      * ```
      * @since v0.4.0
      * @param name Header name
      */
    def removeHeader(name: String): Unit = js.native
    
    val req: Request = js.native
    
    var sendDate: Boolean = js.native
    
    def setHeader(name: String, value: String): this.type = js.native
    def setHeader(name: String, value: js.Array[String]): this.type = js.native
    /**
      * Sets a single header value. If the header already exists in the to-be-sent
      * headers, its value will be replaced. Use an array of strings to send multiple
      * headers with the same name.
      * @since v0.4.0
      * @param name Header name
      * @param value Header value
      */
    def setHeader(name: String, value: Double): this.type = js.native
    
    /**
      * Once a socket is associated with the message and is connected, `socket.setTimeout()` will be called with `msecs` as the first parameter.
      * @since v0.9.12
      * @param callback Optional function to be called when a timeout occurs. Same as binding to the `timeout` event.
      */
    def setTimeout(msecs: Double): this.type = js.native
    def setTimeout(msecs: Double, callback: js.Function0[Unit]): this.type = js.native
    
    var shouldKeepAlive: Boolean = js.native
    
    /**
      * Reference to the underlying socket. Usually, users will not want to access
      * this property.
      *
      * After calling `outgoingMessage.end()`, this property will be nulled.
      * @since v0.3.0
      */
    val socket: Socket | Null = js.native
    
    var useChunkedEncodingByDefault: Boolean = js.native
  }
  
  /**
    * @since v0.1.17
    */
  @JSImport("http", "Server")
  @js.native
  open class Server[Request /* <: Instantiable1[/* socket */ Socket, IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    ServerResponse[IncomingMessage]
  ] */] () extends StObject {
    def this(options: ServerOptions[Request, Response]) = this()
    def this(requestListener: RequestListener[Request, Response]) = this()
    def this(options: ServerOptions[Request, Response], requestListener: RequestListener[Request, Response]) = this()
    
    def addListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_checkContinue(event: checkContinue, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("addListener")
    def addListener_checkExpectation(event: checkExpectation, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("addListener")
    def addListener_clientError(event: clientError, listener: js.Function2[/* err */ js.Error, /* socket */ Duplex, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_connect(
      event: connect,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("addListener")
    def addListener_connection(event: connection, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_dropRequest(
      event: dropRequest,
      listener: js.Function2[/* req */ InstanceType[Request], /* socket */ Duplex, Unit]
    ): this.type = js.native
    @JSName("addListener")
    def addListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_listening(event: listening, listener: js.Function0[Unit]): this.type = js.native
    @JSName("addListener")
    def addListener_request(event: request, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("addListener")
    def addListener_upgrade(
      event: upgrade,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    /**
      * Closes all connections connected to this server.
      * @since v18.2.0
      */
    def closeAllConnections(): Unit = js.native
    
    /**
      * Closes all connections connected to this server which are not sending a request
      * or waiting for a response.
      * @since v18.2.0
      */
    def closeIdleConnections(): Unit = js.native
    
    def emit(event: String, args: Any*): Boolean = js.native
    @JSName("emit")
    def emit_checkContinue(event: checkContinue, req: InstanceType[Request], res: InstanceType[Response] & Req[Request]): Boolean = js.native
    @JSName("emit")
    def emit_checkExpectation(event: checkExpectation, req: InstanceType[Request], res: InstanceType[Response] & Req[Request]): Boolean = js.native
    @JSName("emit")
    def emit_clientError(event: clientError, err: js.Error, socket: Duplex): Boolean = js.native
    @JSName("emit")
    def emit_close(event: close): Boolean = js.native
    @JSName("emit")
    def emit_connect(event: connect, req: InstanceType[Request], socket: Duplex, head: Buffer): Boolean = js.native
    @JSName("emit")
    def emit_connection(event: connection, socket: Socket): Boolean = js.native
    @JSName("emit")
    def emit_dropRequest(event: dropRequest, req: InstanceType[Request], socket: Duplex): Boolean = js.native
    @JSName("emit")
    def emit_error(event: error, err: js.Error): Boolean = js.native
    @JSName("emit")
    def emit_listening(event: listening): Boolean = js.native
    @JSName("emit")
    def emit_request(event: request, req: InstanceType[Request], res: InstanceType[Response] & Req[Request]): Boolean = js.native
    @JSName("emit")
    def emit_upgrade(event: upgrade, req: InstanceType[Request], socket: Duplex, head: Buffer): Boolean = js.native
    
    /**
      * Limit the amount of time the parser will wait to receive the complete HTTP
      * headers.
      *
      * If the timeout expires, the server responds with status 408 without
      * forwarding the request to the request listener and then closes the connection.
      *
      * It must be set to a non-zero value (e.g. 120 seconds) to protect against
      * potential Denial-of-Service attacks in case the server is deployed without a
      * reverse proxy in front.
      * @since v11.3.0, v10.14.0
      */
    var headersTimeout: Double = js.native
    
    /**
      * The number of milliseconds of inactivity a server needs to wait for additional
      * incoming data, after it has finished writing the last response, before a socket
      * will be destroyed. If the server receives new data before the keep-alive
      * timeout has fired, it will reset the regular inactivity timeout, i.e., `server.timeout`.
      *
      * A value of `0` will disable the keep-alive timeout behavior on incoming
      * connections.
      * A value of `0` makes the http server behave similarly to Node.js versions prior
      * to 8.0.0, which did not have a keep-alive timeout.
      *
      * The socket timeout logic is set up on connection, so changing this value only
      * affects new connections to the server, not any existing connections.
      * @since v8.0.0
      */
    var keepAliveTimeout: Double = js.native
    
    /**
      * Limits maximum incoming headers count. If set to 0, no limit will be applied.
      * @since v0.7.0
      */
    var maxHeadersCount: Double | Null = js.native
    
    /**
      * The maximum number of requests socket can handle
      * before closing keep alive connection.
      *
      * A value of `0` will disable the limit.
      *
      * When the limit is reached it will set the `Connection` header value to `close`,
      * but will not actually close the connection, subsequent requests sent
      * after the limit is reached will get `503 Service Unavailable` as a response.
      * @since v16.10.0
      */
    var maxRequestsPerSocket: Double | Null = js.native
    
    def on(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    @JSName("on")
    def on_checkContinue(event: checkContinue, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("on")
    def on_checkExpectation(event: checkExpectation, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("on")
    def on_clientError(event: clientError, listener: js.Function2[/* err */ js.Error, /* socket */ Duplex, Unit]): this.type = js.native
    @JSName("on")
    def on_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("on")
    def on_connect(
      event: connect,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("on")
    def on_connection(event: connection, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("on")
    def on_dropRequest(
      event: dropRequest,
      listener: js.Function2[/* req */ InstanceType[Request], /* socket */ Duplex, Unit]
    ): this.type = js.native
    @JSName("on")
    def on_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("on")
    def on_listening(event: listening, listener: js.Function0[Unit]): this.type = js.native
    @JSName("on")
    def on_request(event: request, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("on")
    def on_upgrade(
      event: upgrade,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    def once(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    @JSName("once")
    def once_checkContinue(event: checkContinue, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("once")
    def once_checkExpectation(event: checkExpectation, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("once")
    def once_clientError(event: clientError, listener: js.Function2[/* err */ js.Error, /* socket */ Duplex, Unit]): this.type = js.native
    @JSName("once")
    def once_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("once")
    def once_connect(
      event: connect,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("once")
    def once_connection(event: connection, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("once")
    def once_dropRequest(
      event: dropRequest,
      listener: js.Function2[/* req */ InstanceType[Request], /* socket */ Duplex, Unit]
    ): this.type = js.native
    @JSName("once")
    def once_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("once")
    def once_listening(event: listening, listener: js.Function0[Unit]): this.type = js.native
    @JSName("once")
    def once_request(event: request, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("once")
    def once_upgrade(
      event: upgrade,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    def prependListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_checkContinue(event: checkContinue, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("prependListener")
    def prependListener_checkExpectation(event: checkExpectation, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("prependListener")
    def prependListener_clientError(event: clientError, listener: js.Function2[/* err */ js.Error, /* socket */ Duplex, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_connect(
      event: connect,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("prependListener")
    def prependListener_connection(event: connection, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_dropRequest(
      event: dropRequest,
      listener: js.Function2[/* req */ InstanceType[Request], /* socket */ Duplex, Unit]
    ): this.type = js.native
    @JSName("prependListener")
    def prependListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_listening(event: listening, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependListener")
    def prependListener_request(event: request, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("prependListener")
    def prependListener_upgrade(
      event: upgrade,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    def prependOnceListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_checkContinue(event: checkContinue, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_checkExpectation(event: checkExpectation, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_clientError(event: clientError, listener: js.Function2[/* err */ js.Error, /* socket */ Duplex, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_connect(
      event: connect,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_connection(event: connection, listener: js.Function1[/* socket */ Socket, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_dropRequest(
      event: dropRequest,
      listener: js.Function2[/* req */ InstanceType[Request], /* socket */ Duplex, Unit]
    ): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_listening(event: listening, listener: js.Function0[Unit]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_request(event: request, listener: RequestListener[Request, Response]): this.type = js.native
    @JSName("prependOnceListener")
    def prependOnceListener_upgrade(
      event: upgrade,
      listener: js.Function3[/* req */ InstanceType[Request], /* socket */ Duplex, /* head */ Buffer, Unit]
    ): this.type = js.native
    
    /**
      * Sets the timeout value in milliseconds for receiving the entire request from
      * the client.
      *
      * If the timeout expires, the server responds with status 408 without
      * forwarding the request to the request listener and then closes the connection.
      *
      * It must be set to a non-zero value (e.g. 120 seconds) to protect against
      * potential Denial-of-Service attacks in case the server is deployed without a
      * reverse proxy in front.
      * @since v14.11.0
      */
    var requestTimeout: Double = js.native
    
    /**
      * Sets the timeout value for sockets, and emits a `'timeout'` event on
      * the Server object, passing the socket as an argument, if a timeout
      * occurs.
      *
      * If there is a `'timeout'` event listener on the Server object, then it
      * will be called with the timed-out socket as an argument.
      *
      * By default, the Server does not timeout sockets. However, if a callback
      * is assigned to the Server's `'timeout'` event, timeouts must be handled
      * explicitly.
      * @since v0.9.12
      * @param [msecs=0 (no timeout)]
      */
    def setTimeout(): this.type = js.native
    def setTimeout(callback: js.Function0[Unit]): this.type = js.native
    def setTimeout(msecs: Double): this.type = js.native
    def setTimeout(msecs: Double, callback: js.Function0[Unit]): this.type = js.native
    def setTimeout(msecs: Unit, callback: js.Function0[Unit]): this.type = js.native
    
    /**
      * The number of milliseconds of inactivity before a socket is presumed
      * to have timed out.
      *
      * A value of `0` will disable the timeout behavior on incoming connections.
      *
      * The socket timeout logic is set up on connection, so changing this
      * value only affects new connections to the server, not any existing connections.
      * @since v0.9.12
      */
    var timeout: Double = js.native
  }
  
  /**
    * This object is created internally by an HTTP server, not by the user. It is
    * passed as the second parameter to the `'request'` event.
    * @since v0.1.17
    */
  @JSImport("http", "ServerResponse")
  @js.native
  open class ServerResponse[Request /* <: IncomingMessage */] protected () extends StObject {
    def this(req: Request) = this()
    
    def assignSocket(socket: Socket): Unit = js.native
    
    def detachSocket(socket: Socket): Unit = js.native
    
    /**
      * When using implicit headers (not calling `response.writeHead()` explicitly),
      * this property controls the status code that will be sent to the client when
      * the headers get flushed.
      *
      * ```js
      * response.statusCode = 404;
      * ```
      *
      * After response header was sent to the client, this property indicates the
      * status code which was sent out.
      * @since v0.4.0
      */
    var statusCode: Double = js.native
    
    /**
      * When using implicit headers (not calling `response.writeHead()` explicitly),
      * this property controls the status message that will be sent to the client when
      * the headers get flushed. If this is left as `undefined` then the standard
      * message for the status code will be used.
      *
      * ```js
      * response.statusMessage = 'Not found';
      * ```
      *
      * After response header was sent to the client, this property indicates the
      * status message which was sent out.
      * @since v0.11.8
      */
    var statusMessage: String = js.native
    
    /**
      * If set to `true`, Node.js will check whether the `Content-Length` header value and the size of the body, in bytes, are equal.
      * Mismatching the `Content-Length` header value will result
      * in an `Error` being thrown, identified by `code:``'ERR_HTTP_CONTENT_LENGTH_MISMATCH'`.
      * @since v18.10.0, v16.18.0
      */
    var strictContentLength: Boolean = js.native
    
    /**
      * Sends an HTTP/1.1 100 Continue message to the client, indicating that
      * the request body should be sent. See the `'checkContinue'` event on `Server`.
      * @since v0.3.0
      */
    def writeContinue(): Unit = js.native
    def writeContinue(callback: js.Function0[Unit]): Unit = js.native
    
    /**
      * Sends an HTTP/1.1 103 Early Hints message to the client with a Link header,
      * indicating that the user agent can preload/preconnect the linked resources.
      * The `hints` is an object containing the values of headers to be sent with
      * early hints message. The optional `callback` argument will be called when
      * the response message has been written.
      *
      * **Example**
      *
      * ```js
      * const earlyHintsLink = '</styles.css>; rel=preload; as=style';
      * response.writeEarlyHints({
      *   'link': earlyHintsLink,
      * });
      *
      * const earlyHintsLinks = [
      *   '</styles.css>; rel=preload; as=style',
      *   '</scripts.js>; rel=preload; as=script',
      * ];
      * response.writeEarlyHints({
      *   'link': earlyHintsLinks,
      *   'x-trace-id': 'id for diagnostics',
      * });
      *
      * const earlyHintsCallback = () => console.log('early hints message sent');
      * response.writeEarlyHints({
      *   'link': earlyHintsLinks,
      * }, earlyHintsCallback);
      * ```
      * @since v18.11.0
      * @param hints An object containing the values of headers
      * @param callback Will be called when the response message has been written
      */
    def writeEarlyHints(hints: Record[String, String | js.Array[String]]): Unit = js.native
    def writeEarlyHints(hints: Record[String, String | js.Array[String]], callback: js.Function0[Unit]): Unit = js.native
    
    /**
      * Sends a response header to the request. The status code is a 3-digit HTTP
      * status code, like `404`. The last argument, `headers`, are the response headers.
      * Optionally one can give a human-readable `statusMessage` as the second
      * argument.
      *
      * `headers` may be an `Array` where the keys and values are in the same list.
      * It is _not_ a list of tuples. So, the even-numbered offsets are key values,
      * and the odd-numbered offsets are the associated values. The array is in the same
      * format as `request.rawHeaders`.
      *
      * Returns a reference to the `ServerResponse`, so that calls can be chained.
      *
      * ```js
      * const body = 'hello world';
      * response
      *   .writeHead(200, {
      *     'Content-Length': Buffer.byteLength(body),
      *     'Content-Type': 'text/plain',
      *   })
      *   .end(body);
      * ```
      *
      * This method must only be called once on a message and it must
      * be called before `response.end()` is called.
      *
      * If `response.write()` or `response.end()` are called before calling
      * this, the implicit/mutable headers will be calculated and call this function.
      *
      * When headers have been set with `response.setHeader()`, they will be merged
      * with any headers passed to `response.writeHead()`, with the headers passed
      * to `response.writeHead()` given precedence.
      *
      * If this method is called and `response.setHeader()` has not been called,
      * it will directly write the supplied header values onto the network channel
      * without caching internally, and the `response.getHeader()` on the header
      * will not yield the expected result. If progressive population of headers is
      * desired with potential future retrieval and modification, use `response.setHeader()` instead.
      *
      * ```js
      * // Returns content-type = text/plain
      * const server = http.createServer((req, res) => {
      *   res.setHeader('Content-Type', 'text/html');
      *   res.setHeader('X-Foo', 'bar');
      *   res.writeHead(200, { 'Content-Type': 'text/plain' });
      *   res.end('ok');
      * });
      * ```
      *
      * `Content-Length` is read in bytes, not characters. Use `Buffer.byteLength()` to determine the length of the body in bytes. Node.js
      * will check whether `Content-Length` and the length of the body which has
      * been transmitted are equal or not.
      *
      * Attempting to set a header field name or value that contains invalid characters
      * will result in a \[`Error`\]\[\] being thrown.
      * @since v0.1.30
      */
    def writeHead(statusCode: Double): this.type = js.native
    def writeHead(statusCode: Double, headers: js.Array[OutgoingHttpHeader]): this.type = js.native
    def writeHead(statusCode: Double, headers: OutgoingHttpHeaders): this.type = js.native
    def writeHead(statusCode: Double, statusMessage: String): this.type = js.native
    def writeHead(statusCode: Double, statusMessage: String, headers: js.Array[OutgoingHttpHeader]): this.type = js.native
    def writeHead(statusCode: Double, statusMessage: String, headers: OutgoingHttpHeaders): this.type = js.native
    def writeHead(statusCode: Double, statusMessage: Unit, headers: js.Array[OutgoingHttpHeader]): this.type = js.native
    def writeHead(statusCode: Double, statusMessage: Unit, headers: OutgoingHttpHeaders): this.type = js.native
    
    /**
      * Sends a HTTP/1.1 102 Processing message to the client, indicating that
      * the request body should be sent.
      * @since v10.0.0
      */
    def writeProcessing(): Unit = js.native
  }
  
  /**
    * Returns a new instance of {@link Server}.
    *
    * The `requestListener` is a function which is automatically
    * added to the `'request'` event.
    *
    * ```js
    * import http from 'node:http';
    *
    * // Create a local server to receive data from
    * const server = http.createServer((req, res) => {
    *   res.writeHead(200, { 'Content-Type': 'application/json' });
    *   res.end(JSON.stringify({
    *     data: 'Hello World!',
    *   }));
    * });
    *
    * server.listen(8000);
    * ```
    *
    * ```js
    * import http from 'node:http';
    *
    * // Create a local server to receive data from
    * const server = http.createServer();
    *
    * // Listen to the request event
    * server.on('request', (request, res) => {
    *   res.writeHead(200, { 'Content-Type': 'application/json' });
    *   res.end(JSON.stringify({
    *     data: 'Hello World!',
    *   }));
    * });
    *
    * server.listen(8000);
    * ```
    * @since v0.1.13
    */
  inline def createServer[Request /* <: Instantiable1[/* socket */ Socket, IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    ServerResponse[IncomingMessage]
  ] */](): Server[Request, Response] = ^.asInstanceOf[js.Dynamic].applyDynamic("createServer")().asInstanceOf[Server[Request, Response]]
  inline def createServer[Request /* <: Instantiable1[/* socket */ Socket, IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    ServerResponse[IncomingMessage]
  ] */](options: ServerOptions[Request, Response]): Server[Request, Response] = ^.asInstanceOf[js.Dynamic].applyDynamic("createServer")(options.asInstanceOf[js.Any]).asInstanceOf[Server[Request, Response]]
  inline def createServer[Request /* <: Instantiable1[/* socket */ Socket, IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    ServerResponse[IncomingMessage]
  ] */](options: ServerOptions[Request, Response], requestListener: RequestListener[Request, Response]): Server[Request, Response] = (^.asInstanceOf[js.Dynamic].applyDynamic("createServer")(options.asInstanceOf[js.Any], requestListener.asInstanceOf[js.Any])).asInstanceOf[Server[Request, Response]]
  inline def createServer[Request /* <: Instantiable1[/* socket */ Socket, IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    ServerResponse[IncomingMessage]
  ] */](requestListener: RequestListener[Request, Response]): Server[Request, Response] = ^.asInstanceOf[js.Dynamic].applyDynamic("createServer")(requestListener.asInstanceOf[js.Any]).asInstanceOf[Server[Request, Response]]
  
  inline def get(options: String): ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any]).asInstanceOf[ClientRequest]
  inline def get(options: String, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  /**
    * Since most requests are GET requests without bodies, Node.js provides this
    * convenience method. The only difference between this method and {@link request} is that it sets the method to GET by default and calls `req.end()` automatically. The callback must take care to
    * consume the response
    * data for reasons stated in {@link ClientRequest} section.
    *
    * The `callback` is invoked with a single argument that is an instance of {@link IncomingMessage}.
    *
    * JSON fetching example:
    *
    * ```js
    * http.get('http://localhost:8000/', (res) => {
    *   const { statusCode } = res;
    *   const contentType = res.headers['content-type'];
    *
    *   let error;
    *   // Any 2xx status code signals a successful response but
    *   // here we're only checking for 200.
    *   if (statusCode !== 200) {
    *     error = new Error('Request Failed.\n' +
    *                       `Status Code: ${statusCode}`);
    *   } else if (!/^application\/json/.test(contentType)) {
    *     error = new Error('Invalid content-type.\n' +
    *                       `Expected application/json but received ${contentType}`);
    *   }
    *   if (error) {
    *     console.error(error.message);
    *     // Consume response data to free up memory
    *     res.resume();
    *     return;
    *   }
    *
    *   res.setEncoding('utf8');
    *   let rawData = '';
    *   res.on('data', (chunk) => { rawData += chunk; });
    *   res.on('end', () => {
    *     try {
    *       const parsedData = JSON.parse(rawData);
    *       console.log(parsedData);
    *     } catch (e) {
    *       console.error(e.message);
    *     }
    *   });
    * }).on('error', (e) => {
    *   console.error(`Got error: ${e.message}`);
    * });
    *
    * // Create a local server to receive data from
    * const server = http.createServer((req, res) => {
    *   res.writeHead(200, { 'Content-Type': 'application/json' });
    *   res.end(JSON.stringify({
    *     data: 'Hello World!',
    *   }));
    * });
    *
    * server.listen(8000);
    * ```
    * @since v0.3.6
    * @param options Accepts the same `options` as {@link request}, with the method set to GET by default.
    */
  inline def get(options: RequestOptions): ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any]).asInstanceOf[ClientRequest]
  inline def get(options: RequestOptions, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def get(options: URL): ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any]).asInstanceOf[ClientRequest]
  inline def get(options: URL, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def get(url: String, options: RequestOptions): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def get(url: String, options: RequestOptions, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def get(url: URL, options: RequestOptions): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def get(url: URL, options: RequestOptions, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  
  /**
    * Global instance of `Agent` which is used as the default for all HTTP client
    * requests. Diverges from a default `Agent` configuration by having `keepAlive`
    * enabled and a `timeout` of 5 seconds.
    * @since v0.5.9
    */
  @JSImport("http", "globalAgent")
  @js.native
  def globalAgent: Agent = js.native
  inline def globalAgent_=(x: Agent): Unit = ^.asInstanceOf[js.Dynamic].updateDynamic("globalAgent")(x.asInstanceOf[js.Any])
  
  /**
    * Read-only property specifying the maximum allowed size of HTTP headers in bytes.
    * Defaults to 16KB. Configurable using the `--max-http-header-size` CLI option.
    */
  @JSImport("http", "maxHeaderSize")
  @js.native
  val maxHeaderSize: Double = js.native
  
  inline def request(options: String): ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any]).asInstanceOf[ClientRequest]
  inline def request(options: String, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  /**
    * `options` in `socket.connect()` are also supported.
    *
    * Node.js maintains several connections per server to make HTTP requests.
    * This function allows one to transparently issue requests.
    *
    * `url` can be a string or a `URL` object. If `url` is a
    * string, it is automatically parsed with `new URL()`. If it is a `URL` object, it will be automatically converted to an ordinary `options` object.
    *
    * If both `url` and `options` are specified, the objects are merged, with the `options` properties taking precedence.
    *
    * The optional `callback` parameter will be added as a one-time listener for
    * the `'response'` event.
    *
    * `http.request()` returns an instance of the {@link ClientRequest} class. The `ClientRequest` instance is a writable stream. If one needs to
    * upload a file with a POST request, then write to the `ClientRequest` object.
    *
    * ```js
    * import http from 'node:http';
    * import { Buffer } from 'node:buffer';
    *
    * const postData = JSON.stringify({
    *   'msg': 'Hello World!',
    * });
    *
    * const options = {
    *   hostname: 'www.google.com',
    *   port: 80,
    *   path: '/upload',
    *   method: 'POST',
    *   headers: {
    *     'Content-Type': 'application/json',
    *     'Content-Length': Buffer.byteLength(postData),
    *   },
    * };
    *
    * const req = http.request(options, (res) => {
    *   console.log(`STATUS: ${res.statusCode}`);
    *   console.log(`HEADERS: ${JSON.stringify(res.headers)}`);
    *   res.setEncoding('utf8');
    *   res.on('data', (chunk) => {
    *     console.log(`BODY: ${chunk}`);
    *   });
    *   res.on('end', () => {
    *     console.log('No more data in response.');
    *   });
    * });
    *
    * req.on('error', (e) => {
    *   console.error(`problem with request: ${e.message}`);
    * });
    *
    * // Write data to request body
    * req.write(postData);
    * req.end();
    * ```
    *
    * In the example `req.end()` was called. With `http.request()` one
    * must always call `req.end()` to signify the end of the request -
    * even if there is no data being written to the request body.
    *
    * If any error is encountered during the request (be that with DNS resolution,
    * TCP level errors, or actual HTTP parse errors) an `'error'` event is emitted
    * on the returned request object. As with all `'error'` events, if no listeners
    * are registered the error will be thrown.
    *
    * There are a few special headers that should be noted.
    *
    * * Sending a 'Connection: keep-alive' will notify Node.js that the connection to
    * the server should be persisted until the next request.
    * * Sending a 'Content-Length' header will disable the default chunked encoding.
    * * Sending an 'Expect' header will immediately send the request headers.
    * Usually, when sending 'Expect: 100-continue', both a timeout and a listener
    * for the `'continue'` event should be set. See RFC 2616 Section 8.2.3 for more
    * information.
    * * Sending an Authorization header will override using the `auth` option
    * to compute basic authentication.
    *
    * Example using a `URL` as `options`:
    *
    * ```js
    * const options = new URL('http://abc:xyz@example.com');
    *
    * const req = http.request(options, (res) => {
    *   // ...
    * });
    * ```
    *
    * In a successful request, the following events will be emitted in the following
    * order:
    *
    * * `'socket'`
    * * `'response'`
    *    * `'data'` any number of times, on the `res` object
    *    (`'data'` will not be emitted at all if the response body is empty, for
    *    instance, in most redirects)
    *    * `'end'` on the `res` object
    * * `'close'`
    *
    * In the case of a connection error, the following events will be emitted:
    *
    * * `'socket'`
    * * `'error'`
    * * `'close'`
    *
    * In the case of a premature connection close before the response is received,
    * the following events will be emitted in the following order:
    *
    * * `'socket'`
    * * `'error'` with an error with message `'Error: socket hang up'` and code `'ECONNRESET'`
    * * `'close'`
    *
    * In the case of a premature connection close after the response is received,
    * the following events will be emitted in the following order:
    *
    * * `'socket'`
    * * `'response'`
    *    * `'data'` any number of times, on the `res` object
    * * (connection closed here)
    * * `'aborted'` on the `res` object
    * * `'close'`
    * * `'error'` on the `res` object with an error with message `'Error: aborted'` and code `'ECONNRESET'`
    * * `'close'` on the `res` object
    *
    * If `req.destroy()` is called before a socket is assigned, the following
    * events will be emitted in the following order:
    *
    * * (`req.destroy()` called here)
    * * `'error'` with an error with message `'Error: socket hang up'` and code `'ECONNRESET'`, or the error with which `req.destroy()` was called
    * * `'close'`
    *
    * If `req.destroy()` is called before the connection succeeds, the following
    * events will be emitted in the following order:
    *
    * * `'socket'`
    * * (`req.destroy()` called here)
    * * `'error'` with an error with message `'Error: socket hang up'` and code `'ECONNRESET'`, or the error with which `req.destroy()` was called
    * * `'close'`
    *
    * If `req.destroy()` is called after the response is received, the following
    * events will be emitted in the following order:
    *
    * * `'socket'`
    * * `'response'`
    *    * `'data'` any number of times, on the `res` object
    * * (`req.destroy()` called here)
    * * `'aborted'` on the `res` object
    * * `'close'`
    * * `'error'` on the `res` object with an error with message `'Error: aborted'` and code `'ECONNRESET'`, or the error with which `req.destroy()` was called
    * * `'close'` on the `res` object
    *
    * If `req.abort()` is called before a socket is assigned, the following
    * events will be emitted in the following order:
    *
    * * (`req.abort()` called here)
    * * `'abort'`
    * * `'close'`
    *
    * If `req.abort()` is called before the connection succeeds, the following
    * events will be emitted in the following order:
    *
    * * `'socket'`
    * * (`req.abort()` called here)
    * * `'abort'`
    * * `'error'` with an error with message `'Error: socket hang up'` and code `'ECONNRESET'`
    * * `'close'`
    *
    * If `req.abort()` is called after the response is received, the following
    * events will be emitted in the following order:
    *
    * * `'socket'`
    * * `'response'`
    *    * `'data'` any number of times, on the `res` object
    * * (`req.abort()` called here)
    * * `'abort'`
    * * `'aborted'` on the `res` object
    * * `'error'` on the `res` object with an error with message `'Error: aborted'` and code `'ECONNRESET'`.
    * * `'close'`
    * * `'close'` on the `res` object
    *
    * Setting the `timeout` option or using the `setTimeout()` function will
    * not abort the request or do anything besides add a `'timeout'` event.
    *
    * Passing an `AbortSignal` and then calling `abort()` on the corresponding `AbortController` will behave the same way as calling `.destroy()` on the
    * request. Specifically, the `'error'` event will be emitted with an error with
    * the message `'AbortError: The operation was aborted'`, the code `'ABORT_ERR'` and the `cause`, if one was provided.
    * @since v0.3.6
    */
  inline def request(options: RequestOptions): ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any]).asInstanceOf[ClientRequest]
  inline def request(options: RequestOptions, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def request(options: URL): ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any]).asInstanceOf[ClientRequest]
  inline def request(options: URL, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def request(url: String, options: RequestOptions): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def request(url: String, options: RequestOptions, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def request(url: URL, options: RequestOptions): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  inline def request(url: URL, options: RequestOptions, callback: js.Function1[/* res */ IncomingMessage, Unit]): ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[ClientRequest]
  
  /**
    * Set the maximum number of idle HTTP parsers.
    * @since v18.8.0, v16.18.0
    * @param [max=1000]
    */
  inline def setMaxIdleHTTPParsers(max: Double): Unit = ^.asInstanceOf[js.Dynamic].applyDynamic("setMaxIdleHTTPParsers")(max.asInstanceOf[js.Any]).asInstanceOf[Unit]
  
  /**
    * Performs the low-level validations on the provided `name` that are done when `res.setHeader(name, value)` is called.
    *
    * Passing illegal value as `name` will result in a `TypeError` being thrown,
    * identified by `code: 'ERR_INVALID_HTTP_TOKEN'`.
    *
    * It is not necessary to use this method before passing headers to an HTTP request
    * or response. The HTTP module will automatically validate such headers.
    *
    * Example:
    *
    * ```js
    * import { validateHeaderName } from 'node:http';
    *
    * try {
    *   validateHeaderName('');
    * } catch (err) {
    *   console.error(err instanceof TypeError); // --> true
    *   console.error(err.code); // --> 'ERR_INVALID_HTTP_TOKEN'
    *   console.error(err.message); // --> 'Header name must be a valid HTTP token [""]'
    * }
    * ```
    * @since v14.3.0
    * @param [label='Header name'] Label for error message.
    */
  inline def validateHeaderName(name: String): Unit = ^.asInstanceOf[js.Dynamic].applyDynamic("validateHeaderName")(name.asInstanceOf[js.Any]).asInstanceOf[Unit]
  
  /**
    * Performs the low-level validations on the provided `value` that are done when `res.setHeader(name, value)` is called.
    *
    * Passing illegal value as `value` will result in a `TypeError` being thrown.
    *
    * * Undefined value error is identified by `code: 'ERR_HTTP_INVALID_HEADER_VALUE'`.
    * * Invalid value character error is identified by `code: 'ERR_INVALID_CHAR'`.
    *
    * It is not necessary to use this method before passing headers to an HTTP request
    * or response. The HTTP module will automatically validate such headers.
    *
    * Examples:
    *
    * ```js
    * import { validateHeaderValue } from 'node:http';
    *
    * try {
    *   validateHeaderValue('x-my-header', undefined);
    * } catch (err) {
    *   console.error(err instanceof TypeError); // --> true
    *   console.error(err.code === 'ERR_HTTP_INVALID_HEADER_VALUE'); // --> true
    *   console.error(err.message); // --> 'Invalid value "undefined" for header "x-my-header"'
    * }
    *
    * try {
    *   validateHeaderValue('x-my-header', 'oʊmɪɡə');
    * } catch (err) {
    *   console.error(err instanceof TypeError); // --> true
    *   console.error(err.code === 'ERR_INVALID_CHAR'); // --> true
    *   console.error(err.message); // --> 'Invalid character in header content ["x-my-header"]'
    * }
    * ```
    * @since v14.3.0
    * @param name Header name
    * @param value Header value
    */
  inline def validateHeaderValue(name: String, value: String): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("validateHeaderValue")(name.asInstanceOf[js.Any], value.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  /* Inlined parent std.Partial<node.node:net.TcpSocketConnectOpts> */
  trait AgentOptions extends StObject {
    
    var autoSelectFamily: js.UndefOr[Boolean] = js.undefined
    
    var autoSelectFamilyAttemptTimeout: js.UndefOr[Double] = js.undefined
    
    var family: js.UndefOr[Double] = js.undefined
    
    var hints: js.UndefOr[Double] = js.undefined
    
    var host: js.UndefOr[String] = js.undefined
    
    /**
      * Keep sockets around in a pool to be used by other requests in the future. Default = false
      */
    var keepAlive: js.UndefOr[Boolean] = js.undefined
    
    var keepAliveInitialDelay: js.UndefOr[Double] = js.undefined
    
    /**
      * When using HTTP KeepAlive, how often to send TCP KeepAlive packets over sockets being kept alive. Default = 1000.
      * Only relevant if keepAlive is set to true.
      */
    var keepAliveMsecs: js.UndefOr[Double] = js.undefined
    
    var localAddress: js.UndefOr[String] = js.undefined
    
    var localPort: js.UndefOr[Double] = js.undefined
    
    var lookup: js.UndefOr[LookupFunction] = js.undefined
    
    /**
      * Maximum number of sockets to leave open in a free state. Only relevant if keepAlive is set to true. Default = 256.
      */
    var maxFreeSockets: js.UndefOr[Double] = js.undefined
    
    /**
      * Maximum number of sockets to allow per host. Default for Node 0.10 is 5, default for Node 0.12 is Infinity
      */
    var maxSockets: js.UndefOr[Double] = js.undefined
    
    /**
      * Maximum number of sockets allowed for all hosts in total. Each request will use a new socket until the maximum is reached. Default: Infinity.
      */
    var maxTotalSockets: js.UndefOr[Double] = js.undefined
    
    var noDelay: js.UndefOr[Boolean] = js.undefined
    
    var onread: js.UndefOr[OnReadOpts] = js.undefined
    
    var port: js.UndefOr[Double] = js.undefined
    
    /**
      * Scheduling strategy to apply when picking the next free socket to use.
      * @default `lifo`
      */
    var scheduling: js.UndefOr[fifo | lifo] = js.undefined
    
    /**
      * Socket timeout in milliseconds. This will set the timeout after the socket is connected.
      */
    var timeout: js.UndefOr[Double] = js.undefined
  }
  object AgentOptions {
    
    inline def apply(): AgentOptions = {
      val __obj = js.Dynamic.literal()
      __obj.asInstanceOf[AgentOptions]
    }
    
    @scala.inline
    implicit open class MutableBuilder[Self <: AgentOptions] (val x: Self) extends AnyVal {
      
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
      
      inline def setKeepAliveMsecs(value: Double): Self = StObject.set(x, "keepAliveMsecs", value.asInstanceOf[js.Any])
      
      inline def setKeepAliveMsecsUndefined: Self = StObject.set(x, "keepAliveMsecs", js.undefined)
      
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
      
      inline def setMaxFreeSockets(value: Double): Self = StObject.set(x, "maxFreeSockets", value.asInstanceOf[js.Any])
      
      inline def setMaxFreeSocketsUndefined: Self = StObject.set(x, "maxFreeSockets", js.undefined)
      
      inline def setMaxSockets(value: Double): Self = StObject.set(x, "maxSockets", value.asInstanceOf[js.Any])
      
      inline def setMaxSocketsUndefined: Self = StObject.set(x, "maxSockets", js.undefined)
      
      inline def setMaxTotalSockets(value: Double): Self = StObject.set(x, "maxTotalSockets", value.asInstanceOf[js.Any])
      
      inline def setMaxTotalSocketsUndefined: Self = StObject.set(x, "maxTotalSockets", js.undefined)
      
      inline def setNoDelay(value: Boolean): Self = StObject.set(x, "noDelay", value.asInstanceOf[js.Any])
      
      inline def setNoDelayUndefined: Self = StObject.set(x, "noDelay", js.undefined)
      
      inline def setOnread(value: OnReadOpts): Self = StObject.set(x, "onread", value.asInstanceOf[js.Any])
      
      inline def setOnreadUndefined: Self = StObject.set(x, "onread", js.undefined)
      
      inline def setPort(value: Double): Self = StObject.set(x, "port", value.asInstanceOf[js.Any])
      
      inline def setPortUndefined: Self = StObject.set(x, "port", js.undefined)
      
      inline def setScheduling(value: fifo | lifo): Self = StObject.set(x, "scheduling", value.asInstanceOf[js.Any])
      
      inline def setSchedulingUndefined: Self = StObject.set(x, "scheduling", js.undefined)
      
      inline def setTimeout(value: Double): Self = StObject.set(x, "timeout", value.asInstanceOf[js.Any])
      
      inline def setTimeoutUndefined: Self = StObject.set(x, "timeout", js.undefined)
    }
  }
  
  trait ClientRequestArgs extends StObject {
    
    var _defaultAgent: js.UndefOr[Agent] = js.undefined
    
    var agent: js.UndefOr[Agent | Boolean] = js.undefined
    
    var auth: js.UndefOr[String | Null] = js.undefined
    
    var createConnection: js.UndefOr[
        js.Function2[
          /* options */ this.type, 
          /* oncreate */ js.Function2[/* err */ js.Error | Null, /* socket */ Duplex, Unit], 
          js.UndefOr[Duplex | Null]
        ]
      ] = js.undefined
    
    var defaultPort: js.UndefOr[Double | String] = js.undefined
    
    var family: js.UndefOr[Double] = js.undefined
    
    var headers: js.UndefOr[OutgoingHttpHeaders] = js.undefined
    
    var hints: js.UndefOr[Double] = js.undefined
    
    var host: js.UndefOr[String | Null] = js.undefined
    
    var hostname: js.UndefOr[String | Null] = js.undefined
    
    var insecureHTTPParser: js.UndefOr[Boolean] = js.undefined
    
    var joinDuplicateHeaders: js.UndefOr[Boolean] = js.undefined
    
    var localAddress: js.UndefOr[String] = js.undefined
    
    var localPort: js.UndefOr[Double] = js.undefined
    
    var lookup: js.UndefOr[LookupFunction] = js.undefined
    
    /**
      * @default 16384
      */
    var maxHeaderSize: js.UndefOr[Double] = js.undefined
    
    var method: js.UndefOr[String] = js.undefined
    
    var path: js.UndefOr[String | Null] = js.undefined
    
    var port: js.UndefOr[Double | String | Null] = js.undefined
    
    var protocol: js.UndefOr[String | Null] = js.undefined
    
    var setHost: js.UndefOr[Boolean] = js.undefined
    
    var signal: js.UndefOr[AbortSignal] = js.undefined
    
    var socketPath: js.UndefOr[String] = js.undefined
    
    var timeout: js.UndefOr[Double] = js.undefined
    
    var uniqueHeaders: js.UndefOr[js.Array[String | js.Array[String]]] = js.undefined
  }
  object ClientRequestArgs {
    
    inline def apply(): ClientRequestArgs = {
      val __obj = js.Dynamic.literal()
      __obj.asInstanceOf[ClientRequestArgs]
    }
    
    @scala.inline
    implicit open class MutableBuilder[Self <: ClientRequestArgs] (val x: Self) extends AnyVal {
      
      inline def setAgent(value: Agent | Boolean): Self = StObject.set(x, "agent", value.asInstanceOf[js.Any])
      
      inline def setAgentUndefined: Self = StObject.set(x, "agent", js.undefined)
      
      inline def setAuth(value: String): Self = StObject.set(x, "auth", value.asInstanceOf[js.Any])
      
      inline def setAuthNull: Self = StObject.set(x, "auth", null)
      
      inline def setAuthUndefined: Self = StObject.set(x, "auth", js.undefined)
      
      inline def setCreateConnection(
        value: (ClientRequestArgs, /* oncreate */ js.Function2[/* err */ js.Error | Null, /* socket */ Duplex, Unit]) => js.UndefOr[Duplex | Null]
      ): Self = StObject.set(x, "createConnection", js.Any.fromFunction2(value))
      
      inline def setCreateConnectionUndefined: Self = StObject.set(x, "createConnection", js.undefined)
      
      inline def setDefaultPort(value: Double | String): Self = StObject.set(x, "defaultPort", value.asInstanceOf[js.Any])
      
      inline def setDefaultPortUndefined: Self = StObject.set(x, "defaultPort", js.undefined)
      
      inline def setFamily(value: Double): Self = StObject.set(x, "family", value.asInstanceOf[js.Any])
      
      inline def setFamilyUndefined: Self = StObject.set(x, "family", js.undefined)
      
      inline def setHeaders(value: OutgoingHttpHeaders): Self = StObject.set(x, "headers", value.asInstanceOf[js.Any])
      
      inline def setHeadersUndefined: Self = StObject.set(x, "headers", js.undefined)
      
      inline def setHints(value: Double): Self = StObject.set(x, "hints", value.asInstanceOf[js.Any])
      
      inline def setHintsUndefined: Self = StObject.set(x, "hints", js.undefined)
      
      inline def setHost(value: String): Self = StObject.set(x, "host", value.asInstanceOf[js.Any])
      
      inline def setHostNull: Self = StObject.set(x, "host", null)
      
      inline def setHostUndefined: Self = StObject.set(x, "host", js.undefined)
      
      inline def setHostname(value: String): Self = StObject.set(x, "hostname", value.asInstanceOf[js.Any])
      
      inline def setHostnameNull: Self = StObject.set(x, "hostname", null)
      
      inline def setHostnameUndefined: Self = StObject.set(x, "hostname", js.undefined)
      
      inline def setInsecureHTTPParser(value: Boolean): Self = StObject.set(x, "insecureHTTPParser", value.asInstanceOf[js.Any])
      
      inline def setInsecureHTTPParserUndefined: Self = StObject.set(x, "insecureHTTPParser", js.undefined)
      
      inline def setJoinDuplicateHeaders(value: Boolean): Self = StObject.set(x, "joinDuplicateHeaders", value.asInstanceOf[js.Any])
      
      inline def setJoinDuplicateHeadersUndefined: Self = StObject.set(x, "joinDuplicateHeaders", js.undefined)
      
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
      
      inline def setMaxHeaderSize(value: Double): Self = StObject.set(x, "maxHeaderSize", value.asInstanceOf[js.Any])
      
      inline def setMaxHeaderSizeUndefined: Self = StObject.set(x, "maxHeaderSize", js.undefined)
      
      inline def setMethod(value: String): Self = StObject.set(x, "method", value.asInstanceOf[js.Any])
      
      inline def setMethodUndefined: Self = StObject.set(x, "method", js.undefined)
      
      inline def setPath(value: String): Self = StObject.set(x, "path", value.asInstanceOf[js.Any])
      
      inline def setPathNull: Self = StObject.set(x, "path", null)
      
      inline def setPathUndefined: Self = StObject.set(x, "path", js.undefined)
      
      inline def setPort(value: Double | String): Self = StObject.set(x, "port", value.asInstanceOf[js.Any])
      
      inline def setPortNull: Self = StObject.set(x, "port", null)
      
      inline def setPortUndefined: Self = StObject.set(x, "port", js.undefined)
      
      inline def setProtocol(value: String): Self = StObject.set(x, "protocol", value.asInstanceOf[js.Any])
      
      inline def setProtocolNull: Self = StObject.set(x, "protocol", null)
      
      inline def setProtocolUndefined: Self = StObject.set(x, "protocol", js.undefined)
      
      inline def setSetHost(value: Boolean): Self = StObject.set(x, "setHost", value.asInstanceOf[js.Any])
      
      inline def setSetHostUndefined: Self = StObject.set(x, "setHost", js.undefined)
      
      inline def setSignal(value: AbortSignal): Self = StObject.set(x, "signal", value.asInstanceOf[js.Any])
      
      inline def setSignalUndefined: Self = StObject.set(x, "signal", js.undefined)
      
      inline def setSocketPath(value: String): Self = StObject.set(x, "socketPath", value.asInstanceOf[js.Any])
      
      inline def setSocketPathUndefined: Self = StObject.set(x, "socketPath", js.undefined)
      
      inline def setTimeout(value: Double): Self = StObject.set(x, "timeout", value.asInstanceOf[js.Any])
      
      inline def setTimeoutUndefined: Self = StObject.set(x, "timeout", js.undefined)
      
      inline def setUniqueHeaders(value: js.Array[String | js.Array[String]]): Self = StObject.set(x, "uniqueHeaders", value.asInstanceOf[js.Any])
      
      inline def setUniqueHeadersUndefined: Self = StObject.set(x, "uniqueHeaders", js.undefined)
      
      inline def setUniqueHeadersVarargs(value: (String | js.Array[String])*): Self = StObject.set(x, "uniqueHeaders", js.Array(value*))
      
      inline def set_defaultAgent(value: Agent): Self = StObject.set(x, "_defaultAgent", value.asInstanceOf[js.Any])
      
      inline def set_defaultAgentUndefined: Self = StObject.set(x, "_defaultAgent", js.undefined)
    }
  }
  
  // incoming headers will never contain number
  trait IncomingHttpHeaders
    extends StObject
       with Dict[String | js.Array[String]] {
    
    var accept: js.UndefOr[String] = js.undefined
    
    var `accept-language`: js.UndefOr[String] = js.undefined
    
    var `accept-patch`: js.UndefOr[String] = js.undefined
    
    var `accept-ranges`: js.UndefOr[String] = js.undefined
    
    var `access-control-allow-credentials`: js.UndefOr[String] = js.undefined
    
    var `access-control-allow-headers`: js.UndefOr[String] = js.undefined
    
    var `access-control-allow-methods`: js.UndefOr[String] = js.undefined
    
    var `access-control-allow-origin`: js.UndefOr[String] = js.undefined
    
    var `access-control-expose-headers`: js.UndefOr[String] = js.undefined
    
    var `access-control-max-age`: js.UndefOr[String] = js.undefined
    
    var `access-control-request-headers`: js.UndefOr[String] = js.undefined
    
    var `access-control-request-method`: js.UndefOr[String] = js.undefined
    
    var age: js.UndefOr[String] = js.undefined
    
    var allow: js.UndefOr[String] = js.undefined
    
    var `alt-svc`: js.UndefOr[String] = js.undefined
    
    var authorization: js.UndefOr[String] = js.undefined
    
    var `cache-control`: js.UndefOr[String] = js.undefined
    
    var connection: js.UndefOr[String] = js.undefined
    
    var `content-disposition`: js.UndefOr[String] = js.undefined
    
    var `content-encoding`: js.UndefOr[String] = js.undefined
    
    var `content-language`: js.UndefOr[String] = js.undefined
    
    var `content-length`: js.UndefOr[String] = js.undefined
    
    var `content-location`: js.UndefOr[String] = js.undefined
    
    var `content-range`: js.UndefOr[String] = js.undefined
    
    var `content-type`: js.UndefOr[String] = js.undefined
    
    var cookie: js.UndefOr[String] = js.undefined
    
    var date: js.UndefOr[String] = js.undefined
    
    var etag: js.UndefOr[String] = js.undefined
    
    var expect: js.UndefOr[String] = js.undefined
    
    var expires: js.UndefOr[String] = js.undefined
    
    var forwarded: js.UndefOr[String] = js.undefined
    
    var from: js.UndefOr[String] = js.undefined
    
    var host: js.UndefOr[String] = js.undefined
    
    var `if-match`: js.UndefOr[String] = js.undefined
    
    var `if-modified-since`: js.UndefOr[String] = js.undefined
    
    var `if-none-match`: js.UndefOr[String] = js.undefined
    
    var `if-unmodified-since`: js.UndefOr[String] = js.undefined
    
    var `last-modified`: js.UndefOr[String] = js.undefined
    
    var location: js.UndefOr[String] = js.undefined
    
    var origin: js.UndefOr[String] = js.undefined
    
    var pragma: js.UndefOr[String] = js.undefined
    
    var `proxy-authenticate`: js.UndefOr[String] = js.undefined
    
    var `proxy-authorization`: js.UndefOr[String] = js.undefined
    
    var `public-key-pins`: js.UndefOr[String] = js.undefined
    
    var range: js.UndefOr[String] = js.undefined
    
    var referer: js.UndefOr[String] = js.undefined
    
    var `retry-after`: js.UndefOr[String] = js.undefined
    
    var `sec-websocket-accept`: js.UndefOr[String] = js.undefined
    
    var `sec-websocket-extensions`: js.UndefOr[String] = js.undefined
    
    var `sec-websocket-key`: js.UndefOr[String] = js.undefined
    
    var `sec-websocket-protocol`: js.UndefOr[String] = js.undefined
    
    var `sec-websocket-version`: js.UndefOr[String] = js.undefined
    
    var `set-cookie`: js.UndefOr[js.Array[String]] = js.undefined
    
    var `strict-transport-security`: js.UndefOr[String] = js.undefined
    
    var tk: js.UndefOr[String] = js.undefined
    
    var trailer: js.UndefOr[String] = js.undefined
    
    var `transfer-encoding`: js.UndefOr[String] = js.undefined
    
    var upgrade: js.UndefOr[String] = js.undefined
    
    var `user-agent`: js.UndefOr[String] = js.undefined
    
    var vary: js.UndefOr[String] = js.undefined
    
    var via: js.UndefOr[String] = js.undefined
    
    var warning: js.UndefOr[String] = js.undefined
    
    var `www-authenticate`: js.UndefOr[String] = js.undefined
  }
  object IncomingHttpHeaders {
    
    inline def apply(): IncomingHttpHeaders = {
      val __obj = js.Dynamic.literal()
      __obj.asInstanceOf[IncomingHttpHeaders]
    }
    
    @scala.inline
    implicit open class MutableBuilder[Self <: IncomingHttpHeaders] (val x: Self) extends AnyVal {
      
      inline def setAccept(value: String): Self = StObject.set(x, "accept", value.asInstanceOf[js.Any])
      
      inline def `setAccept-language`(value: String): Self = StObject.set(x, "accept-language", value.asInstanceOf[js.Any])
      
      inline def `setAccept-languageUndefined`: Self = StObject.set(x, "accept-language", js.undefined)
      
      inline def `setAccept-patch`(value: String): Self = StObject.set(x, "accept-patch", value.asInstanceOf[js.Any])
      
      inline def `setAccept-patchUndefined`: Self = StObject.set(x, "accept-patch", js.undefined)
      
      inline def `setAccept-ranges`(value: String): Self = StObject.set(x, "accept-ranges", value.asInstanceOf[js.Any])
      
      inline def `setAccept-rangesUndefined`: Self = StObject.set(x, "accept-ranges", js.undefined)
      
      inline def setAcceptUndefined: Self = StObject.set(x, "accept", js.undefined)
      
      inline def `setAccess-control-allow-credentials`(value: String): Self = StObject.set(x, "access-control-allow-credentials", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-allow-credentialsUndefined`: Self = StObject.set(x, "access-control-allow-credentials", js.undefined)
      
      inline def `setAccess-control-allow-headers`(value: String): Self = StObject.set(x, "access-control-allow-headers", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-allow-headersUndefined`: Self = StObject.set(x, "access-control-allow-headers", js.undefined)
      
      inline def `setAccess-control-allow-methods`(value: String): Self = StObject.set(x, "access-control-allow-methods", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-allow-methodsUndefined`: Self = StObject.set(x, "access-control-allow-methods", js.undefined)
      
      inline def `setAccess-control-allow-origin`(value: String): Self = StObject.set(x, "access-control-allow-origin", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-allow-originUndefined`: Self = StObject.set(x, "access-control-allow-origin", js.undefined)
      
      inline def `setAccess-control-expose-headers`(value: String): Self = StObject.set(x, "access-control-expose-headers", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-expose-headersUndefined`: Self = StObject.set(x, "access-control-expose-headers", js.undefined)
      
      inline def `setAccess-control-max-age`(value: String): Self = StObject.set(x, "access-control-max-age", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-max-ageUndefined`: Self = StObject.set(x, "access-control-max-age", js.undefined)
      
      inline def `setAccess-control-request-headers`(value: String): Self = StObject.set(x, "access-control-request-headers", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-request-headersUndefined`: Self = StObject.set(x, "access-control-request-headers", js.undefined)
      
      inline def `setAccess-control-request-method`(value: String): Self = StObject.set(x, "access-control-request-method", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-request-methodUndefined`: Self = StObject.set(x, "access-control-request-method", js.undefined)
      
      inline def setAge(value: String): Self = StObject.set(x, "age", value.asInstanceOf[js.Any])
      
      inline def setAgeUndefined: Self = StObject.set(x, "age", js.undefined)
      
      inline def setAllow(value: String): Self = StObject.set(x, "allow", value.asInstanceOf[js.Any])
      
      inline def setAllowUndefined: Self = StObject.set(x, "allow", js.undefined)
      
      inline def `setAlt-svc`(value: String): Self = StObject.set(x, "alt-svc", value.asInstanceOf[js.Any])
      
      inline def `setAlt-svcUndefined`: Self = StObject.set(x, "alt-svc", js.undefined)
      
      inline def setAuthorization(value: String): Self = StObject.set(x, "authorization", value.asInstanceOf[js.Any])
      
      inline def setAuthorizationUndefined: Self = StObject.set(x, "authorization", js.undefined)
      
      inline def `setCache-control`(value: String): Self = StObject.set(x, "cache-control", value.asInstanceOf[js.Any])
      
      inline def `setCache-controlUndefined`: Self = StObject.set(x, "cache-control", js.undefined)
      
      inline def setConnection(value: String): Self = StObject.set(x, "connection", value.asInstanceOf[js.Any])
      
      inline def setConnectionUndefined: Self = StObject.set(x, "connection", js.undefined)
      
      inline def `setContent-disposition`(value: String): Self = StObject.set(x, "content-disposition", value.asInstanceOf[js.Any])
      
      inline def `setContent-dispositionUndefined`: Self = StObject.set(x, "content-disposition", js.undefined)
      
      inline def `setContent-encoding`(value: String): Self = StObject.set(x, "content-encoding", value.asInstanceOf[js.Any])
      
      inline def `setContent-encodingUndefined`: Self = StObject.set(x, "content-encoding", js.undefined)
      
      inline def `setContent-language`(value: String): Self = StObject.set(x, "content-language", value.asInstanceOf[js.Any])
      
      inline def `setContent-languageUndefined`: Self = StObject.set(x, "content-language", js.undefined)
      
      inline def `setContent-length`(value: String): Self = StObject.set(x, "content-length", value.asInstanceOf[js.Any])
      
      inline def `setContent-lengthUndefined`: Self = StObject.set(x, "content-length", js.undefined)
      
      inline def `setContent-location`(value: String): Self = StObject.set(x, "content-location", value.asInstanceOf[js.Any])
      
      inline def `setContent-locationUndefined`: Self = StObject.set(x, "content-location", js.undefined)
      
      inline def `setContent-range`(value: String): Self = StObject.set(x, "content-range", value.asInstanceOf[js.Any])
      
      inline def `setContent-rangeUndefined`: Self = StObject.set(x, "content-range", js.undefined)
      
      inline def `setContent-type`(value: String): Self = StObject.set(x, "content-type", value.asInstanceOf[js.Any])
      
      inline def `setContent-typeUndefined`: Self = StObject.set(x, "content-type", js.undefined)
      
      inline def setCookie(value: String): Self = StObject.set(x, "cookie", value.asInstanceOf[js.Any])
      
      inline def setCookieUndefined: Self = StObject.set(x, "cookie", js.undefined)
      
      inline def setDate(value: String): Self = StObject.set(x, "date", value.asInstanceOf[js.Any])
      
      inline def setDateUndefined: Self = StObject.set(x, "date", js.undefined)
      
      inline def setEtag(value: String): Self = StObject.set(x, "etag", value.asInstanceOf[js.Any])
      
      inline def setEtagUndefined: Self = StObject.set(x, "etag", js.undefined)
      
      inline def setExpect(value: String): Self = StObject.set(x, "expect", value.asInstanceOf[js.Any])
      
      inline def setExpectUndefined: Self = StObject.set(x, "expect", js.undefined)
      
      inline def setExpires(value: String): Self = StObject.set(x, "expires", value.asInstanceOf[js.Any])
      
      inline def setExpiresUndefined: Self = StObject.set(x, "expires", js.undefined)
      
      inline def setForwarded(value: String): Self = StObject.set(x, "forwarded", value.asInstanceOf[js.Any])
      
      inline def setForwardedUndefined: Self = StObject.set(x, "forwarded", js.undefined)
      
      inline def setFrom(value: String): Self = StObject.set(x, "from", value.asInstanceOf[js.Any])
      
      inline def setFromUndefined: Self = StObject.set(x, "from", js.undefined)
      
      inline def setHost(value: String): Self = StObject.set(x, "host", value.asInstanceOf[js.Any])
      
      inline def setHostUndefined: Self = StObject.set(x, "host", js.undefined)
      
      inline def `setIf-match`(value: String): Self = StObject.set(x, "if-match", value.asInstanceOf[js.Any])
      
      inline def `setIf-matchUndefined`: Self = StObject.set(x, "if-match", js.undefined)
      
      inline def `setIf-modified-since`(value: String): Self = StObject.set(x, "if-modified-since", value.asInstanceOf[js.Any])
      
      inline def `setIf-modified-sinceUndefined`: Self = StObject.set(x, "if-modified-since", js.undefined)
      
      inline def `setIf-none-match`(value: String): Self = StObject.set(x, "if-none-match", value.asInstanceOf[js.Any])
      
      inline def `setIf-none-matchUndefined`: Self = StObject.set(x, "if-none-match", js.undefined)
      
      inline def `setIf-unmodified-since`(value: String): Self = StObject.set(x, "if-unmodified-since", value.asInstanceOf[js.Any])
      
      inline def `setIf-unmodified-sinceUndefined`: Self = StObject.set(x, "if-unmodified-since", js.undefined)
      
      inline def `setLast-modified`(value: String): Self = StObject.set(x, "last-modified", value.asInstanceOf[js.Any])
      
      inline def `setLast-modifiedUndefined`: Self = StObject.set(x, "last-modified", js.undefined)
      
      inline def setLocation(value: String): Self = StObject.set(x, "location", value.asInstanceOf[js.Any])
      
      inline def setLocationUndefined: Self = StObject.set(x, "location", js.undefined)
      
      inline def setOrigin(value: String): Self = StObject.set(x, "origin", value.asInstanceOf[js.Any])
      
      inline def setOriginUndefined: Self = StObject.set(x, "origin", js.undefined)
      
      inline def setPragma(value: String): Self = StObject.set(x, "pragma", value.asInstanceOf[js.Any])
      
      inline def setPragmaUndefined: Self = StObject.set(x, "pragma", js.undefined)
      
      inline def `setProxy-authenticate`(value: String): Self = StObject.set(x, "proxy-authenticate", value.asInstanceOf[js.Any])
      
      inline def `setProxy-authenticateUndefined`: Self = StObject.set(x, "proxy-authenticate", js.undefined)
      
      inline def `setProxy-authorization`(value: String): Self = StObject.set(x, "proxy-authorization", value.asInstanceOf[js.Any])
      
      inline def `setProxy-authorizationUndefined`: Self = StObject.set(x, "proxy-authorization", js.undefined)
      
      inline def `setPublic-key-pins`(value: String): Self = StObject.set(x, "public-key-pins", value.asInstanceOf[js.Any])
      
      inline def `setPublic-key-pinsUndefined`: Self = StObject.set(x, "public-key-pins", js.undefined)
      
      inline def setRange(value: String): Self = StObject.set(x, "range", value.asInstanceOf[js.Any])
      
      inline def setRangeUndefined: Self = StObject.set(x, "range", js.undefined)
      
      inline def setReferer(value: String): Self = StObject.set(x, "referer", value.asInstanceOf[js.Any])
      
      inline def setRefererUndefined: Self = StObject.set(x, "referer", js.undefined)
      
      inline def `setRetry-after`(value: String): Self = StObject.set(x, "retry-after", value.asInstanceOf[js.Any])
      
      inline def `setRetry-afterUndefined`: Self = StObject.set(x, "retry-after", js.undefined)
      
      inline def `setSec-websocket-accept`(value: String): Self = StObject.set(x, "sec-websocket-accept", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-acceptUndefined`: Self = StObject.set(x, "sec-websocket-accept", js.undefined)
      
      inline def `setSec-websocket-extensions`(value: String): Self = StObject.set(x, "sec-websocket-extensions", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-extensionsUndefined`: Self = StObject.set(x, "sec-websocket-extensions", js.undefined)
      
      inline def `setSec-websocket-key`(value: String): Self = StObject.set(x, "sec-websocket-key", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-keyUndefined`: Self = StObject.set(x, "sec-websocket-key", js.undefined)
      
      inline def `setSec-websocket-protocol`(value: String): Self = StObject.set(x, "sec-websocket-protocol", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-protocolUndefined`: Self = StObject.set(x, "sec-websocket-protocol", js.undefined)
      
      inline def `setSec-websocket-version`(value: String): Self = StObject.set(x, "sec-websocket-version", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-versionUndefined`: Self = StObject.set(x, "sec-websocket-version", js.undefined)
      
      inline def `setSet-cookie`(value: js.Array[String]): Self = StObject.set(x, "set-cookie", value.asInstanceOf[js.Any])
      
      inline def `setSet-cookieUndefined`: Self = StObject.set(x, "set-cookie", js.undefined)
      
      inline def `setSet-cookieVarargs`(value: String*): Self = StObject.set(x, "set-cookie", js.Array(value*))
      
      inline def `setStrict-transport-security`(value: String): Self = StObject.set(x, "strict-transport-security", value.asInstanceOf[js.Any])
      
      inline def `setStrict-transport-securityUndefined`: Self = StObject.set(x, "strict-transport-security", js.undefined)
      
      inline def setTk(value: String): Self = StObject.set(x, "tk", value.asInstanceOf[js.Any])
      
      inline def setTkUndefined: Self = StObject.set(x, "tk", js.undefined)
      
      inline def setTrailer(value: String): Self = StObject.set(x, "trailer", value.asInstanceOf[js.Any])
      
      inline def setTrailerUndefined: Self = StObject.set(x, "trailer", js.undefined)
      
      inline def `setTransfer-encoding`(value: String): Self = StObject.set(x, "transfer-encoding", value.asInstanceOf[js.Any])
      
      inline def `setTransfer-encodingUndefined`: Self = StObject.set(x, "transfer-encoding", js.undefined)
      
      inline def setUpgrade(value: String): Self = StObject.set(x, "upgrade", value.asInstanceOf[js.Any])
      
      inline def setUpgradeUndefined: Self = StObject.set(x, "upgrade", js.undefined)
      
      inline def `setUser-agent`(value: String): Self = StObject.set(x, "user-agent", value.asInstanceOf[js.Any])
      
      inline def `setUser-agentUndefined`: Self = StObject.set(x, "user-agent", js.undefined)
      
      inline def setVary(value: String): Self = StObject.set(x, "vary", value.asInstanceOf[js.Any])
      
      inline def setVaryUndefined: Self = StObject.set(x, "vary", js.undefined)
      
      inline def setVia(value: String): Self = StObject.set(x, "via", value.asInstanceOf[js.Any])
      
      inline def setViaUndefined: Self = StObject.set(x, "via", js.undefined)
      
      inline def setWarning(value: String): Self = StObject.set(x, "warning", value.asInstanceOf[js.Any])
      
      inline def setWarningUndefined: Self = StObject.set(x, "warning", js.undefined)
      
      inline def `setWww-authenticate`(value: String): Self = StObject.set(x, "www-authenticate", value.asInstanceOf[js.Any])
      
      inline def `setWww-authenticateUndefined`: Self = StObject.set(x, "www-authenticate", js.undefined)
    }
  }
  
  trait InformationEvent extends StObject {
    
    var headers: IncomingHttpHeaders
    
    var httpVersion: String
    
    var httpVersionMajor: Double
    
    var httpVersionMinor: Double
    
    var rawHeaders: js.Array[String]
    
    var statusCode: Double
    
    var statusMessage: String
  }
  object InformationEvent {
    
    inline def apply(
      headers: IncomingHttpHeaders,
      httpVersion: String,
      httpVersionMajor: Double,
      httpVersionMinor: Double,
      rawHeaders: js.Array[String],
      statusCode: Double,
      statusMessage: String
    ): InformationEvent = {
      val __obj = js.Dynamic.literal(headers = headers.asInstanceOf[js.Any], httpVersion = httpVersion.asInstanceOf[js.Any], httpVersionMajor = httpVersionMajor.asInstanceOf[js.Any], httpVersionMinor = httpVersionMinor.asInstanceOf[js.Any], rawHeaders = rawHeaders.asInstanceOf[js.Any], statusCode = statusCode.asInstanceOf[js.Any], statusMessage = statusMessage.asInstanceOf[js.Any])
      __obj.asInstanceOf[InformationEvent]
    }
    
    @scala.inline
    implicit open class MutableBuilder[Self <: InformationEvent] (val x: Self) extends AnyVal {
      
      inline def setHeaders(value: IncomingHttpHeaders): Self = StObject.set(x, "headers", value.asInstanceOf[js.Any])
      
      inline def setHttpVersion(value: String): Self = StObject.set(x, "httpVersion", value.asInstanceOf[js.Any])
      
      inline def setHttpVersionMajor(value: Double): Self = StObject.set(x, "httpVersionMajor", value.asInstanceOf[js.Any])
      
      inline def setHttpVersionMinor(value: Double): Self = StObject.set(x, "httpVersionMinor", value.asInstanceOf[js.Any])
      
      inline def setRawHeaders(value: js.Array[String]): Self = StObject.set(x, "rawHeaders", value.asInstanceOf[js.Any])
      
      inline def setRawHeadersVarargs(value: String*): Self = StObject.set(x, "rawHeaders", js.Array(value*))
      
      inline def setStatusCode(value: Double): Self = StObject.set(x, "statusCode", value.asInstanceOf[js.Any])
      
      inline def setStatusMessage(value: String): Self = StObject.set(x, "statusMessage", value.asInstanceOf[js.Any])
    }
  }
  
  // outgoing headers allows numbers (as they are converted internally to strings)
  type OutgoingHttpHeader = Double | String | js.Array[String]
  
  trait OutgoingHttpHeaders
    extends StObject
       with Dict[OutgoingHttpHeader] {
    
    var accept: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `accept-charset`: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `accept-encoding`: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `accept-language`: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `accept-ranges`: js.UndefOr[String] = js.undefined
    
    var `access-control-allow-credentials`: js.UndefOr[String] = js.undefined
    
    var `access-control-allow-headers`: js.UndefOr[String] = js.undefined
    
    var `access-control-allow-methods`: js.UndefOr[String] = js.undefined
    
    var `access-control-allow-origin`: js.UndefOr[String] = js.undefined
    
    var `access-control-expose-headers`: js.UndefOr[String] = js.undefined
    
    var `access-control-max-age`: js.UndefOr[String] = js.undefined
    
    var `access-control-request-headers`: js.UndefOr[String] = js.undefined
    
    var `access-control-request-method`: js.UndefOr[String] = js.undefined
    
    var age: js.UndefOr[String] = js.undefined
    
    var allow: js.UndefOr[String] = js.undefined
    
    var authorization: js.UndefOr[String] = js.undefined
    
    var `cache-control`: js.UndefOr[String] = js.undefined
    
    var `cdn-cache-control`: js.UndefOr[String] = js.undefined
    
    var connection: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `content-disposition`: js.UndefOr[String] = js.undefined
    
    var `content-encoding`: js.UndefOr[String] = js.undefined
    
    var `content-language`: js.UndefOr[String] = js.undefined
    
    var `content-length`: js.UndefOr[String | Double] = js.undefined
    
    var `content-location`: js.UndefOr[String] = js.undefined
    
    var `content-range`: js.UndefOr[String] = js.undefined
    
    var `content-security-policy`: js.UndefOr[String] = js.undefined
    
    var `content-security-policy-report-only`: js.UndefOr[String] = js.undefined
    
    var cookie: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var date: js.UndefOr[String] = js.undefined
    
    var dav: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var dnt: js.UndefOr[String] = js.undefined
    
    var etag: js.UndefOr[String] = js.undefined
    
    var expect: js.UndefOr[String] = js.undefined
    
    var expires: js.UndefOr[String] = js.undefined
    
    var forwarded: js.UndefOr[String] = js.undefined
    
    var from: js.UndefOr[String] = js.undefined
    
    var host: js.UndefOr[String] = js.undefined
    
    var `if-match`: js.UndefOr[String] = js.undefined
    
    var `if-modified-since`: js.UndefOr[String] = js.undefined
    
    var `if-none-match`: js.UndefOr[String] = js.undefined
    
    var `if-range`: js.UndefOr[String] = js.undefined
    
    var `if-unmodified-since`: js.UndefOr[String] = js.undefined
    
    var `last-modified`: js.UndefOr[String] = js.undefined
    
    var link: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var location: js.UndefOr[String] = js.undefined
    
    var `max-forwards`: js.UndefOr[String] = js.undefined
    
    var origin: js.UndefOr[String] = js.undefined
    
    var prgama: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `proxy-authenticate`: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `proxy-authorization`: js.UndefOr[String] = js.undefined
    
    var `public-key-pins`: js.UndefOr[String] = js.undefined
    
    var `public-key-pins-report-only`: js.UndefOr[String] = js.undefined
    
    var range: js.UndefOr[String] = js.undefined
    
    var referer: js.UndefOr[String] = js.undefined
    
    var `referrer-policy`: js.UndefOr[String] = js.undefined
    
    var refresh: js.UndefOr[String] = js.undefined
    
    var `retry-after`: js.UndefOr[String] = js.undefined
    
    var `sec-websocket-accept`: js.UndefOr[String] = js.undefined
    
    var `sec-websocket-extensions`: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `sec-websocket-key`: js.UndefOr[String] = js.undefined
    
    var `sec-websocket-protocol`: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `sec-websocket-version`: js.UndefOr[String] = js.undefined
    
    var server: js.UndefOr[String] = js.undefined
    
    var `set-cookie`: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `strict-transport-security`: js.UndefOr[String] = js.undefined
    
    var te: js.UndefOr[String] = js.undefined
    
    var trailer: js.UndefOr[String] = js.undefined
    
    var `transfer-encoding`: js.UndefOr[String] = js.undefined
    
    var upgrade: js.UndefOr[String] = js.undefined
    
    var `upgrade-insecure-requests`: js.UndefOr[String] = js.undefined
    
    var `user-agent`: js.UndefOr[String] = js.undefined
    
    var vary: js.UndefOr[String] = js.undefined
    
    var via: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var warning: js.UndefOr[String] = js.undefined
    
    var `www-authenticate`: js.UndefOr[String | js.Array[String]] = js.undefined
    
    var `x-content-type-options`: js.UndefOr[String] = js.undefined
    
    var `x-dns-prefetch-control`: js.UndefOr[String] = js.undefined
    
    var `x-frame-options`: js.UndefOr[String] = js.undefined
    
    var `x-xss-protection`: js.UndefOr[String] = js.undefined
  }
  object OutgoingHttpHeaders {
    
    inline def apply(): OutgoingHttpHeaders = {
      val __obj = js.Dynamic.literal()
      __obj.asInstanceOf[OutgoingHttpHeaders]
    }
    
    @scala.inline
    implicit open class MutableBuilder[Self <: OutgoingHttpHeaders] (val x: Self) extends AnyVal {
      
      inline def setAccept(value: String | js.Array[String]): Self = StObject.set(x, "accept", value.asInstanceOf[js.Any])
      
      inline def `setAccept-charset`(value: String | js.Array[String]): Self = StObject.set(x, "accept-charset", value.asInstanceOf[js.Any])
      
      inline def `setAccept-charsetUndefined`: Self = StObject.set(x, "accept-charset", js.undefined)
      
      inline def `setAccept-charsetVarargs`(value: String*): Self = StObject.set(x, "accept-charset", js.Array(value*))
      
      inline def `setAccept-encoding`(value: String | js.Array[String]): Self = StObject.set(x, "accept-encoding", value.asInstanceOf[js.Any])
      
      inline def `setAccept-encodingUndefined`: Self = StObject.set(x, "accept-encoding", js.undefined)
      
      inline def `setAccept-encodingVarargs`(value: String*): Self = StObject.set(x, "accept-encoding", js.Array(value*))
      
      inline def `setAccept-language`(value: String | js.Array[String]): Self = StObject.set(x, "accept-language", value.asInstanceOf[js.Any])
      
      inline def `setAccept-languageUndefined`: Self = StObject.set(x, "accept-language", js.undefined)
      
      inline def `setAccept-languageVarargs`(value: String*): Self = StObject.set(x, "accept-language", js.Array(value*))
      
      inline def `setAccept-ranges`(value: String): Self = StObject.set(x, "accept-ranges", value.asInstanceOf[js.Any])
      
      inline def `setAccept-rangesUndefined`: Self = StObject.set(x, "accept-ranges", js.undefined)
      
      inline def setAcceptUndefined: Self = StObject.set(x, "accept", js.undefined)
      
      inline def setAcceptVarargs(value: String*): Self = StObject.set(x, "accept", js.Array(value*))
      
      inline def `setAccess-control-allow-credentials`(value: String): Self = StObject.set(x, "access-control-allow-credentials", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-allow-credentialsUndefined`: Self = StObject.set(x, "access-control-allow-credentials", js.undefined)
      
      inline def `setAccess-control-allow-headers`(value: String): Self = StObject.set(x, "access-control-allow-headers", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-allow-headersUndefined`: Self = StObject.set(x, "access-control-allow-headers", js.undefined)
      
      inline def `setAccess-control-allow-methods`(value: String): Self = StObject.set(x, "access-control-allow-methods", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-allow-methodsUndefined`: Self = StObject.set(x, "access-control-allow-methods", js.undefined)
      
      inline def `setAccess-control-allow-origin`(value: String): Self = StObject.set(x, "access-control-allow-origin", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-allow-originUndefined`: Self = StObject.set(x, "access-control-allow-origin", js.undefined)
      
      inline def `setAccess-control-expose-headers`(value: String): Self = StObject.set(x, "access-control-expose-headers", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-expose-headersUndefined`: Self = StObject.set(x, "access-control-expose-headers", js.undefined)
      
      inline def `setAccess-control-max-age`(value: String): Self = StObject.set(x, "access-control-max-age", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-max-ageUndefined`: Self = StObject.set(x, "access-control-max-age", js.undefined)
      
      inline def `setAccess-control-request-headers`(value: String): Self = StObject.set(x, "access-control-request-headers", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-request-headersUndefined`: Self = StObject.set(x, "access-control-request-headers", js.undefined)
      
      inline def `setAccess-control-request-method`(value: String): Self = StObject.set(x, "access-control-request-method", value.asInstanceOf[js.Any])
      
      inline def `setAccess-control-request-methodUndefined`: Self = StObject.set(x, "access-control-request-method", js.undefined)
      
      inline def setAge(value: String): Self = StObject.set(x, "age", value.asInstanceOf[js.Any])
      
      inline def setAgeUndefined: Self = StObject.set(x, "age", js.undefined)
      
      inline def setAllow(value: String): Self = StObject.set(x, "allow", value.asInstanceOf[js.Any])
      
      inline def setAllowUndefined: Self = StObject.set(x, "allow", js.undefined)
      
      inline def setAuthorization(value: String): Self = StObject.set(x, "authorization", value.asInstanceOf[js.Any])
      
      inline def setAuthorizationUndefined: Self = StObject.set(x, "authorization", js.undefined)
      
      inline def `setCache-control`(value: String): Self = StObject.set(x, "cache-control", value.asInstanceOf[js.Any])
      
      inline def `setCache-controlUndefined`: Self = StObject.set(x, "cache-control", js.undefined)
      
      inline def `setCdn-cache-control`(value: String): Self = StObject.set(x, "cdn-cache-control", value.asInstanceOf[js.Any])
      
      inline def `setCdn-cache-controlUndefined`: Self = StObject.set(x, "cdn-cache-control", js.undefined)
      
      inline def setConnection(value: String | js.Array[String]): Self = StObject.set(x, "connection", value.asInstanceOf[js.Any])
      
      inline def setConnectionUndefined: Self = StObject.set(x, "connection", js.undefined)
      
      inline def setConnectionVarargs(value: String*): Self = StObject.set(x, "connection", js.Array(value*))
      
      inline def `setContent-disposition`(value: String): Self = StObject.set(x, "content-disposition", value.asInstanceOf[js.Any])
      
      inline def `setContent-dispositionUndefined`: Self = StObject.set(x, "content-disposition", js.undefined)
      
      inline def `setContent-encoding`(value: String): Self = StObject.set(x, "content-encoding", value.asInstanceOf[js.Any])
      
      inline def `setContent-encodingUndefined`: Self = StObject.set(x, "content-encoding", js.undefined)
      
      inline def `setContent-language`(value: String): Self = StObject.set(x, "content-language", value.asInstanceOf[js.Any])
      
      inline def `setContent-languageUndefined`: Self = StObject.set(x, "content-language", js.undefined)
      
      inline def `setContent-length`(value: String | Double): Self = StObject.set(x, "content-length", value.asInstanceOf[js.Any])
      
      inline def `setContent-lengthUndefined`: Self = StObject.set(x, "content-length", js.undefined)
      
      inline def `setContent-location`(value: String): Self = StObject.set(x, "content-location", value.asInstanceOf[js.Any])
      
      inline def `setContent-locationUndefined`: Self = StObject.set(x, "content-location", js.undefined)
      
      inline def `setContent-range`(value: String): Self = StObject.set(x, "content-range", value.asInstanceOf[js.Any])
      
      inline def `setContent-rangeUndefined`: Self = StObject.set(x, "content-range", js.undefined)
      
      inline def `setContent-security-policy`(value: String): Self = StObject.set(x, "content-security-policy", value.asInstanceOf[js.Any])
      
      inline def `setContent-security-policy-report-only`(value: String): Self = StObject.set(x, "content-security-policy-report-only", value.asInstanceOf[js.Any])
      
      inline def `setContent-security-policy-report-onlyUndefined`: Self = StObject.set(x, "content-security-policy-report-only", js.undefined)
      
      inline def `setContent-security-policyUndefined`: Self = StObject.set(x, "content-security-policy", js.undefined)
      
      inline def setCookie(value: String | js.Array[String]): Self = StObject.set(x, "cookie", value.asInstanceOf[js.Any])
      
      inline def setCookieUndefined: Self = StObject.set(x, "cookie", js.undefined)
      
      inline def setCookieVarargs(value: String*): Self = StObject.set(x, "cookie", js.Array(value*))
      
      inline def setDate(value: String): Self = StObject.set(x, "date", value.asInstanceOf[js.Any])
      
      inline def setDateUndefined: Self = StObject.set(x, "date", js.undefined)
      
      inline def setDav(value: String | js.Array[String]): Self = StObject.set(x, "dav", value.asInstanceOf[js.Any])
      
      inline def setDavUndefined: Self = StObject.set(x, "dav", js.undefined)
      
      inline def setDavVarargs(value: String*): Self = StObject.set(x, "dav", js.Array(value*))
      
      inline def setDnt(value: String): Self = StObject.set(x, "dnt", value.asInstanceOf[js.Any])
      
      inline def setDntUndefined: Self = StObject.set(x, "dnt", js.undefined)
      
      inline def setEtag(value: String): Self = StObject.set(x, "etag", value.asInstanceOf[js.Any])
      
      inline def setEtagUndefined: Self = StObject.set(x, "etag", js.undefined)
      
      inline def setExpect(value: String): Self = StObject.set(x, "expect", value.asInstanceOf[js.Any])
      
      inline def setExpectUndefined: Self = StObject.set(x, "expect", js.undefined)
      
      inline def setExpires(value: String): Self = StObject.set(x, "expires", value.asInstanceOf[js.Any])
      
      inline def setExpiresUndefined: Self = StObject.set(x, "expires", js.undefined)
      
      inline def setForwarded(value: String): Self = StObject.set(x, "forwarded", value.asInstanceOf[js.Any])
      
      inline def setForwardedUndefined: Self = StObject.set(x, "forwarded", js.undefined)
      
      inline def setFrom(value: String): Self = StObject.set(x, "from", value.asInstanceOf[js.Any])
      
      inline def setFromUndefined: Self = StObject.set(x, "from", js.undefined)
      
      inline def setHost(value: String): Self = StObject.set(x, "host", value.asInstanceOf[js.Any])
      
      inline def setHostUndefined: Self = StObject.set(x, "host", js.undefined)
      
      inline def `setIf-match`(value: String): Self = StObject.set(x, "if-match", value.asInstanceOf[js.Any])
      
      inline def `setIf-matchUndefined`: Self = StObject.set(x, "if-match", js.undefined)
      
      inline def `setIf-modified-since`(value: String): Self = StObject.set(x, "if-modified-since", value.asInstanceOf[js.Any])
      
      inline def `setIf-modified-sinceUndefined`: Self = StObject.set(x, "if-modified-since", js.undefined)
      
      inline def `setIf-none-match`(value: String): Self = StObject.set(x, "if-none-match", value.asInstanceOf[js.Any])
      
      inline def `setIf-none-matchUndefined`: Self = StObject.set(x, "if-none-match", js.undefined)
      
      inline def `setIf-range`(value: String): Self = StObject.set(x, "if-range", value.asInstanceOf[js.Any])
      
      inline def `setIf-rangeUndefined`: Self = StObject.set(x, "if-range", js.undefined)
      
      inline def `setIf-unmodified-since`(value: String): Self = StObject.set(x, "if-unmodified-since", value.asInstanceOf[js.Any])
      
      inline def `setIf-unmodified-sinceUndefined`: Self = StObject.set(x, "if-unmodified-since", js.undefined)
      
      inline def `setLast-modified`(value: String): Self = StObject.set(x, "last-modified", value.asInstanceOf[js.Any])
      
      inline def `setLast-modifiedUndefined`: Self = StObject.set(x, "last-modified", js.undefined)
      
      inline def setLink(value: String | js.Array[String]): Self = StObject.set(x, "link", value.asInstanceOf[js.Any])
      
      inline def setLinkUndefined: Self = StObject.set(x, "link", js.undefined)
      
      inline def setLinkVarargs(value: String*): Self = StObject.set(x, "link", js.Array(value*))
      
      inline def setLocation(value: String): Self = StObject.set(x, "location", value.asInstanceOf[js.Any])
      
      inline def setLocationUndefined: Self = StObject.set(x, "location", js.undefined)
      
      inline def `setMax-forwards`(value: String): Self = StObject.set(x, "max-forwards", value.asInstanceOf[js.Any])
      
      inline def `setMax-forwardsUndefined`: Self = StObject.set(x, "max-forwards", js.undefined)
      
      inline def setOrigin(value: String): Self = StObject.set(x, "origin", value.asInstanceOf[js.Any])
      
      inline def setOriginUndefined: Self = StObject.set(x, "origin", js.undefined)
      
      inline def setPrgama(value: String | js.Array[String]): Self = StObject.set(x, "prgama", value.asInstanceOf[js.Any])
      
      inline def setPrgamaUndefined: Self = StObject.set(x, "prgama", js.undefined)
      
      inline def setPrgamaVarargs(value: String*): Self = StObject.set(x, "prgama", js.Array(value*))
      
      inline def `setProxy-authenticate`(value: String | js.Array[String]): Self = StObject.set(x, "proxy-authenticate", value.asInstanceOf[js.Any])
      
      inline def `setProxy-authenticateUndefined`: Self = StObject.set(x, "proxy-authenticate", js.undefined)
      
      inline def `setProxy-authenticateVarargs`(value: String*): Self = StObject.set(x, "proxy-authenticate", js.Array(value*))
      
      inline def `setProxy-authorization`(value: String): Self = StObject.set(x, "proxy-authorization", value.asInstanceOf[js.Any])
      
      inline def `setProxy-authorizationUndefined`: Self = StObject.set(x, "proxy-authorization", js.undefined)
      
      inline def `setPublic-key-pins`(value: String): Self = StObject.set(x, "public-key-pins", value.asInstanceOf[js.Any])
      
      inline def `setPublic-key-pins-report-only`(value: String): Self = StObject.set(x, "public-key-pins-report-only", value.asInstanceOf[js.Any])
      
      inline def `setPublic-key-pins-report-onlyUndefined`: Self = StObject.set(x, "public-key-pins-report-only", js.undefined)
      
      inline def `setPublic-key-pinsUndefined`: Self = StObject.set(x, "public-key-pins", js.undefined)
      
      inline def setRange(value: String): Self = StObject.set(x, "range", value.asInstanceOf[js.Any])
      
      inline def setRangeUndefined: Self = StObject.set(x, "range", js.undefined)
      
      inline def setReferer(value: String): Self = StObject.set(x, "referer", value.asInstanceOf[js.Any])
      
      inline def setRefererUndefined: Self = StObject.set(x, "referer", js.undefined)
      
      inline def `setReferrer-policy`(value: String): Self = StObject.set(x, "referrer-policy", value.asInstanceOf[js.Any])
      
      inline def `setReferrer-policyUndefined`: Self = StObject.set(x, "referrer-policy", js.undefined)
      
      inline def setRefresh(value: String): Self = StObject.set(x, "refresh", value.asInstanceOf[js.Any])
      
      inline def setRefreshUndefined: Self = StObject.set(x, "refresh", js.undefined)
      
      inline def `setRetry-after`(value: String): Self = StObject.set(x, "retry-after", value.asInstanceOf[js.Any])
      
      inline def `setRetry-afterUndefined`: Self = StObject.set(x, "retry-after", js.undefined)
      
      inline def `setSec-websocket-accept`(value: String): Self = StObject.set(x, "sec-websocket-accept", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-acceptUndefined`: Self = StObject.set(x, "sec-websocket-accept", js.undefined)
      
      inline def `setSec-websocket-extensions`(value: String | js.Array[String]): Self = StObject.set(x, "sec-websocket-extensions", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-extensionsUndefined`: Self = StObject.set(x, "sec-websocket-extensions", js.undefined)
      
      inline def `setSec-websocket-extensionsVarargs`(value: String*): Self = StObject.set(x, "sec-websocket-extensions", js.Array(value*))
      
      inline def `setSec-websocket-key`(value: String): Self = StObject.set(x, "sec-websocket-key", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-keyUndefined`: Self = StObject.set(x, "sec-websocket-key", js.undefined)
      
      inline def `setSec-websocket-protocol`(value: String | js.Array[String]): Self = StObject.set(x, "sec-websocket-protocol", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-protocolUndefined`: Self = StObject.set(x, "sec-websocket-protocol", js.undefined)
      
      inline def `setSec-websocket-protocolVarargs`(value: String*): Self = StObject.set(x, "sec-websocket-protocol", js.Array(value*))
      
      inline def `setSec-websocket-version`(value: String): Self = StObject.set(x, "sec-websocket-version", value.asInstanceOf[js.Any])
      
      inline def `setSec-websocket-versionUndefined`: Self = StObject.set(x, "sec-websocket-version", js.undefined)
      
      inline def setServer(value: String): Self = StObject.set(x, "server", value.asInstanceOf[js.Any])
      
      inline def setServerUndefined: Self = StObject.set(x, "server", js.undefined)
      
      inline def `setSet-cookie`(value: String | js.Array[String]): Self = StObject.set(x, "set-cookie", value.asInstanceOf[js.Any])
      
      inline def `setSet-cookieUndefined`: Self = StObject.set(x, "set-cookie", js.undefined)
      
      inline def `setSet-cookieVarargs`(value: String*): Self = StObject.set(x, "set-cookie", js.Array(value*))
      
      inline def `setStrict-transport-security`(value: String): Self = StObject.set(x, "strict-transport-security", value.asInstanceOf[js.Any])
      
      inline def `setStrict-transport-securityUndefined`: Self = StObject.set(x, "strict-transport-security", js.undefined)
      
      inline def setTe(value: String): Self = StObject.set(x, "te", value.asInstanceOf[js.Any])
      
      inline def setTeUndefined: Self = StObject.set(x, "te", js.undefined)
      
      inline def setTrailer(value: String): Self = StObject.set(x, "trailer", value.asInstanceOf[js.Any])
      
      inline def setTrailerUndefined: Self = StObject.set(x, "trailer", js.undefined)
      
      inline def `setTransfer-encoding`(value: String): Self = StObject.set(x, "transfer-encoding", value.asInstanceOf[js.Any])
      
      inline def `setTransfer-encodingUndefined`: Self = StObject.set(x, "transfer-encoding", js.undefined)
      
      inline def setUpgrade(value: String): Self = StObject.set(x, "upgrade", value.asInstanceOf[js.Any])
      
      inline def `setUpgrade-insecure-requests`(value: String): Self = StObject.set(x, "upgrade-insecure-requests", value.asInstanceOf[js.Any])
      
      inline def `setUpgrade-insecure-requestsUndefined`: Self = StObject.set(x, "upgrade-insecure-requests", js.undefined)
      
      inline def setUpgradeUndefined: Self = StObject.set(x, "upgrade", js.undefined)
      
      inline def `setUser-agent`(value: String): Self = StObject.set(x, "user-agent", value.asInstanceOf[js.Any])
      
      inline def `setUser-agentUndefined`: Self = StObject.set(x, "user-agent", js.undefined)
      
      inline def setVary(value: String): Self = StObject.set(x, "vary", value.asInstanceOf[js.Any])
      
      inline def setVaryUndefined: Self = StObject.set(x, "vary", js.undefined)
      
      inline def setVia(value: String | js.Array[String]): Self = StObject.set(x, "via", value.asInstanceOf[js.Any])
      
      inline def setViaUndefined: Self = StObject.set(x, "via", js.undefined)
      
      inline def setViaVarargs(value: String*): Self = StObject.set(x, "via", js.Array(value*))
      
      inline def setWarning(value: String): Self = StObject.set(x, "warning", value.asInstanceOf[js.Any])
      
      inline def setWarningUndefined: Self = StObject.set(x, "warning", js.undefined)
      
      inline def `setWww-authenticate`(value: String | js.Array[String]): Self = StObject.set(x, "www-authenticate", value.asInstanceOf[js.Any])
      
      inline def `setWww-authenticateUndefined`: Self = StObject.set(x, "www-authenticate", js.undefined)
      
      inline def `setWww-authenticateVarargs`(value: String*): Self = StObject.set(x, "www-authenticate", js.Array(value*))
      
      inline def `setX-content-type-options`(value: String): Self = StObject.set(x, "x-content-type-options", value.asInstanceOf[js.Any])
      
      inline def `setX-content-type-optionsUndefined`: Self = StObject.set(x, "x-content-type-options", js.undefined)
      
      inline def `setX-dns-prefetch-control`(value: String): Self = StObject.set(x, "x-dns-prefetch-control", value.asInstanceOf[js.Any])
      
      inline def `setX-dns-prefetch-controlUndefined`: Self = StObject.set(x, "x-dns-prefetch-control", js.undefined)
      
      inline def `setX-frame-options`(value: String): Self = StObject.set(x, "x-frame-options", value.asInstanceOf[js.Any])
      
      inline def `setX-frame-optionsUndefined`: Self = StObject.set(x, "x-frame-options", js.undefined)
      
      inline def `setX-xss-protection`(value: String): Self = StObject.set(x, "x-xss-protection", value.asInstanceOf[js.Any])
      
      inline def `setX-xss-protectionUndefined`: Self = StObject.set(x, "x-xss-protection", js.undefined)
    }
  }
  
  type RequestListener[Request /* <: Instantiable1[/* socket */ Socket, IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    ServerResponse[IncomingMessage]
  ] */] = js.Function2[
    /* req */ InstanceType[Request], 
    /* res */ InstanceType[Response] & Req[Request], 
    Unit
  ]
  
  // although RequestOptions are passed as ClientRequestArgs to ClientRequest directly,
  // create interface RequestOptions would make the naming more clear to developers
  type RequestOptions = ClientRequestArgs
  
  trait ServerOptions[Request /* <: Instantiable1[/* socket */ Socket, IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    ServerResponse[IncomingMessage]
  ] */] extends StObject {
    
    /**
      * Specifies the `IncomingMessage` class to be used. Useful for extending the original `IncomingMessage`.
      */
    var IncomingMessage: js.UndefOr[Request] = js.undefined
    
    /**
      * Specifies the `ServerResponse` class to be used. Useful for extending the original `ServerResponse`.
      */
    var ServerResponse: js.UndefOr[Response] = js.undefined
    
    /**
      * Sets the interval value in milliseconds to check for request and headers timeout in incomplete requests.
      * @default 30000
      */
    var connectionsCheckingInterval: js.UndefOr[Double] = js.undefined
    
    /**
      * Optionally overrides all `socket`s' `readableHighWaterMark` and `writableHighWaterMark`.
      * This affects `highWaterMark` property of both `IncomingMessage` and `ServerResponse`.
      * Default: @see stream.getDefaultHighWaterMark().
      * @since v20.1.0
      */
    var highWaterMark: js.UndefOr[Double] = js.undefined
    
    /**
      * Use an insecure HTTP parser that accepts invalid HTTP headers when `true`.
      * Using the insecure parser should be avoided.
      * See --insecure-http-parser for more information.
      * @default false
      */
    var insecureHTTPParser: js.UndefOr[Boolean] = js.undefined
    
    /**
      * It joins the field line values of multiple headers in a request with `, ` instead of discarding the duplicates.
      * @default false
      * @since v18.14.0
      */
    var joinDuplicateHeaders: js.UndefOr[Boolean] = js.undefined
    
    /**
      * If set to `true`, it enables keep-alive functionality on the socket immediately after a new incoming connection is received,
      * similarly on what is done in `socket.setKeepAlive([enable][, initialDelay])`.
      * @default false
      * @since v16.5.0
      */
    var keepAlive: js.UndefOr[Boolean] = js.undefined
    
    /**
      * If set to a positive number, it sets the initial delay before the first keepalive probe is sent on an idle socket.
      * @default 0
      * @since v16.5.0
      */
    var keepAliveInitialDelay: js.UndefOr[Double] = js.undefined
    
    /**
      * The number of milliseconds of inactivity a server needs to wait for additional incoming data,
      * after it has finished writing the last response, before a socket will be destroyed.
      * @see Server.keepAliveTimeout for more information.
      * @default 5000
      * @since v18.0.0
      */
    var keepAliveTimeout: js.UndefOr[Double] = js.undefined
    
    /**
      * Optionally overrides the value of `--max-http-header-size` for requests received by
      * this server, i.e. the maximum length of request headers in bytes.
      * @default 16384
      * @since v13.3.0
      */
    var maxHeaderSize: js.UndefOr[Double] = js.undefined
    
    /**
      * If set to `true`, it disables the use of Nagle's algorithm immediately after a new incoming connection is received.
      * @default true
      * @since v16.5.0
      */
    var noDelay: js.UndefOr[Boolean] = js.undefined
    
    /**
      * Sets the timeout value in milliseconds for receiving the entire request from the client.
      * @see Server.requestTimeout for more information.
      * @default 300000
      * @since v18.0.0
      */
    var requestTimeout: js.UndefOr[Double] = js.undefined
    
    /**
      * A list of response headers that should be sent only once.
      * If the header's value is an array, the items will be joined using `; `.
      */
    var uniqueHeaders: js.UndefOr[js.Array[String | js.Array[String]]] = js.undefined
  }
  object ServerOptions {
    
    inline def apply[Request /* <: Instantiable1[/* socket */ Socket, IncomingMessage] */, Response /* <: Instantiable1[
        /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
        ServerResponse[IncomingMessage]
      ] */](): ServerOptions[Request, Response] = {
      val __obj = js.Dynamic.literal()
      __obj.asInstanceOf[ServerOptions[Request, Response]]
    }
    
    @scala.inline
    implicit open class MutableBuilder[Self <: ServerOptions[?, ?], Request /* <: Instantiable1[/* socket */ Socket, IncomingMessage] */, Response /* <: Instantiable1[
        /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
        ServerResponse[IncomingMessage]
      ] */] (val x: Self & (ServerOptions[Request, Response])) extends AnyVal {
      
      inline def setConnectionsCheckingInterval(value: Double): Self = StObject.set(x, "connectionsCheckingInterval", value.asInstanceOf[js.Any])
      
      inline def setConnectionsCheckingIntervalUndefined: Self = StObject.set(x, "connectionsCheckingInterval", js.undefined)
      
      inline def setHighWaterMark(value: Double): Self = StObject.set(x, "highWaterMark", value.asInstanceOf[js.Any])
      
      inline def setHighWaterMarkUndefined: Self = StObject.set(x, "highWaterMark", js.undefined)
      
      inline def setIncomingMessage(value: Request): Self = StObject.set(x, "IncomingMessage", value.asInstanceOf[js.Any])
      
      inline def setIncomingMessageUndefined: Self = StObject.set(x, "IncomingMessage", js.undefined)
      
      inline def setInsecureHTTPParser(value: Boolean): Self = StObject.set(x, "insecureHTTPParser", value.asInstanceOf[js.Any])
      
      inline def setInsecureHTTPParserUndefined: Self = StObject.set(x, "insecureHTTPParser", js.undefined)
      
      inline def setJoinDuplicateHeaders(value: Boolean): Self = StObject.set(x, "joinDuplicateHeaders", value.asInstanceOf[js.Any])
      
      inline def setJoinDuplicateHeadersUndefined: Self = StObject.set(x, "joinDuplicateHeaders", js.undefined)
      
      inline def setKeepAlive(value: Boolean): Self = StObject.set(x, "keepAlive", value.asInstanceOf[js.Any])
      
      inline def setKeepAliveInitialDelay(value: Double): Self = StObject.set(x, "keepAliveInitialDelay", value.asInstanceOf[js.Any])
      
      inline def setKeepAliveInitialDelayUndefined: Self = StObject.set(x, "keepAliveInitialDelay", js.undefined)
      
      inline def setKeepAliveTimeout(value: Double): Self = StObject.set(x, "keepAliveTimeout", value.asInstanceOf[js.Any])
      
      inline def setKeepAliveTimeoutUndefined: Self = StObject.set(x, "keepAliveTimeout", js.undefined)
      
      inline def setKeepAliveUndefined: Self = StObject.set(x, "keepAlive", js.undefined)
      
      inline def setMaxHeaderSize(value: Double): Self = StObject.set(x, "maxHeaderSize", value.asInstanceOf[js.Any])
      
      inline def setMaxHeaderSizeUndefined: Self = StObject.set(x, "maxHeaderSize", js.undefined)
      
      inline def setNoDelay(value: Boolean): Self = StObject.set(x, "noDelay", value.asInstanceOf[js.Any])
      
      inline def setNoDelayUndefined: Self = StObject.set(x, "noDelay", js.undefined)
      
      inline def setRequestTimeout(value: Double): Self = StObject.set(x, "requestTimeout", value.asInstanceOf[js.Any])
      
      inline def setRequestTimeoutUndefined: Self = StObject.set(x, "requestTimeout", js.undefined)
      
      inline def setServerResponse(value: Response): Self = StObject.set(x, "ServerResponse", value.asInstanceOf[js.Any])
      
      inline def setServerResponseUndefined: Self = StObject.set(x, "ServerResponse", js.undefined)
      
      inline def setUniqueHeaders(value: js.Array[String | js.Array[String]]): Self = StObject.set(x, "uniqueHeaders", value.asInstanceOf[js.Any])
      
      inline def setUniqueHeadersUndefined: Self = StObject.set(x, "uniqueHeaders", js.undefined)
      
      inline def setUniqueHeadersVarargs(value: (String | js.Array[String])*): Self = StObject.set(x, "uniqueHeaders", js.Array(value*))
    }
  }
}