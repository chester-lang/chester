package typings.node

import org.scalablytyped.runtime.Instantiable1
import typings.node.httpMod.AgentOptions
import typings.node.httpMod.ClientRequestArgs
import typings.node.httpMod.RequestListener
import typings.node.httpMod.RequestOptions
import typings.node.httpMod.ServerOptions
import typings.node.nodeColonnetMod.Socket
import typings.node.nodeColonurlMod.URL
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

object nodeColonhttpMod {
  
  @JSImport("node:http", JSImport.Namespace)
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
  @JSImport("node:http", "Agent")
  @js.native
  open class Agent () extends StObject {
    def this(opts: AgentOptions) = this()
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
  @JSImport("node:http", "ClientRequest")
  @js.native
  open class ClientRequest protected () extends StObject {
    def this(url: String) = this()
    def this(url: ClientRequestArgs) = this()
    def this(url: URL) = this()
    def this(url: String, cb: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]) = this()
    def this(url: ClientRequestArgs, cb: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]) = this()
    def this(url: URL, cb: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]) = this()
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
  @JSImport("node:http", "IncomingMessage")
  @js.native
  open class IncomingMessage protected () extends StObject {
    def this(socket: Socket) = this()
  }
  
  @JSImport("node:http", "METHODS")
  @js.native
  val METHODS: js.Array[String] = js.native
  
  /**
    * This class serves as the parent class of {@link ClientRequest} and {@link ServerResponse}. It is an abstract outgoing message from
    * the perspective of the participants of an HTTP transaction.
    * @since v0.1.17
    */
  @JSImport("node:http", "OutgoingMessage")
  @js.native
  open class OutgoingMessage[Request /* <: typings.node.httpMod.IncomingMessage */] () extends StObject
  
  /**
    * @since v0.1.17
    */
  @JSImport("node:http", "Server")
  @js.native
  open class Server[Request /* <: Instantiable1[/* socket */ Socket, typings.node.httpMod.IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    typings.node.httpMod.ServerResponse[typings.node.httpMod.IncomingMessage]
  ] */] () extends StObject {
    def this(options: ServerOptions[Request, Response]) = this()
    def this(requestListener: RequestListener[Request, Response]) = this()
    def this(options: ServerOptions[Request, Response], requestListener: RequestListener[Request, Response]) = this()
  }
  
  /**
    * This object is created internally by an HTTP server, not by the user. It is
    * passed as the second parameter to the `'request'` event.
    * @since v0.1.17
    */
  @JSImport("node:http", "ServerResponse")
  @js.native
  open class ServerResponse[Request /* <: typings.node.httpMod.IncomingMessage */] protected () extends StObject {
    def this(req: Request) = this()
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
  inline def createServer[Request /* <: Instantiable1[/* socket */ Socket, typings.node.httpMod.IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    typings.node.httpMod.ServerResponse[typings.node.httpMod.IncomingMessage]
  ] */](): typings.node.httpMod.Server[Request, Response] = ^.asInstanceOf[js.Dynamic].applyDynamic("createServer")().asInstanceOf[typings.node.httpMod.Server[Request, Response]]
  inline def createServer[Request /* <: Instantiable1[/* socket */ Socket, typings.node.httpMod.IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    typings.node.httpMod.ServerResponse[typings.node.httpMod.IncomingMessage]
  ] */](options: ServerOptions[Request, Response]): typings.node.httpMod.Server[Request, Response] = ^.asInstanceOf[js.Dynamic].applyDynamic("createServer")(options.asInstanceOf[js.Any]).asInstanceOf[typings.node.httpMod.Server[Request, Response]]
  inline def createServer[Request /* <: Instantiable1[/* socket */ Socket, typings.node.httpMod.IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    typings.node.httpMod.ServerResponse[typings.node.httpMod.IncomingMessage]
  ] */](options: ServerOptions[Request, Response], requestListener: RequestListener[Request, Response]): typings.node.httpMod.Server[Request, Response] = (^.asInstanceOf[js.Dynamic].applyDynamic("createServer")(options.asInstanceOf[js.Any], requestListener.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.Server[Request, Response]]
  inline def createServer[Request /* <: Instantiable1[/* socket */ Socket, typings.node.httpMod.IncomingMessage] */, Response /* <: Instantiable1[
    /* import warning: RewrittenClass.unapply cls was tparam Request */ /* req */ Any, 
    typings.node.httpMod.ServerResponse[typings.node.httpMod.IncomingMessage]
  ] */](requestListener: RequestListener[Request, Response]): typings.node.httpMod.Server[Request, Response] = ^.asInstanceOf[js.Dynamic].applyDynamic("createServer")(requestListener.asInstanceOf[js.Any]).asInstanceOf[typings.node.httpMod.Server[Request, Response]]
  
  inline def get(options: String): typings.node.httpMod.ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any]).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def get(options: String, callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
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
  inline def get(options: RequestOptions): typings.node.httpMod.ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any]).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def get(
    options: RequestOptions,
    callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]
  ): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def get(options: URL): typings.node.httpMod.ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any]).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def get(options: URL, callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def get(url: String, options: RequestOptions): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def get(
    url: String,
    options: RequestOptions,
    callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]
  ): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def get(url: URL, options: RequestOptions): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def get(
    url: URL,
    options: RequestOptions,
    callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]
  ): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("get")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  
  /**
    * Global instance of `Agent` which is used as the default for all HTTP client
    * requests. Diverges from a default `Agent` configuration by having `keepAlive`
    * enabled and a `timeout` of 5 seconds.
    * @since v0.5.9
    */
  @JSImport("node:http", "globalAgent")
  @js.native
  def globalAgent: typings.node.httpMod.Agent = js.native
  inline def globalAgent_=(x: typings.node.httpMod.Agent): Unit = ^.asInstanceOf[js.Dynamic].updateDynamic("globalAgent")(x.asInstanceOf[js.Any])
  
  /**
    * Read-only property specifying the maximum allowed size of HTTP headers in bytes.
    * Defaults to 16KB. Configurable using the `--max-http-header-size` CLI option.
    */
  @JSImport("node:http", "maxHeaderSize")
  @js.native
  val maxHeaderSize: Double = js.native
  
  inline def request(options: String): typings.node.httpMod.ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any]).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def request(options: String, callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
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
  inline def request(options: RequestOptions): typings.node.httpMod.ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any]).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def request(
    options: RequestOptions,
    callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]
  ): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def request(options: URL): typings.node.httpMod.ClientRequest = ^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any]).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def request(options: URL, callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def request(url: String, options: RequestOptions): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def request(
    url: String,
    options: RequestOptions,
    callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]
  ): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def request(url: URL, options: RequestOptions): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  inline def request(
    url: URL,
    options: RequestOptions,
    callback: js.Function1[/* res */ typings.node.httpMod.IncomingMessage, Unit]
  ): typings.node.httpMod.ClientRequest = (^.asInstanceOf[js.Dynamic].applyDynamic("request")(url.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[typings.node.httpMod.ClientRequest]
  
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
}
