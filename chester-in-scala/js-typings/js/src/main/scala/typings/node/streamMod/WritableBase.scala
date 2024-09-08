package typings.node.streamMod

import typings.node.anon.Chunk
import typings.node.bufferMod.global.BufferEncoding
import typings.node.nodeStrings.close
import typings.node.nodeStrings.drain
import typings.node.nodeStrings.error
import typings.node.nodeStrings.finish
import typings.node.nodeStrings.pipe
import typings.node.nodeStrings.unpipe
import typings.std.WritableStream
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

@js.native
trait WritableBase
  extends StObject
     with WritableStream[Any] {
  
  var _construct: js.UndefOr[
    js.Function1[/* callback */ js.Function1[/* error */ js.UndefOr[js.Error | Null], Unit], Unit]
  ] = js.native
  
  def _destroy(error: js.Error, callback: js.Function1[/* error */ js.UndefOr[js.Error | Null], Unit]): Unit = js.native
  def _destroy(error: Null, callback: js.Function1[/* error */ js.UndefOr[js.Error | Null], Unit]): Unit = js.native
  
  def _final(callback: js.Function1[/* error */ js.UndefOr[js.Error | Null], Unit]): Unit = js.native
  
  def _write(
    chunk: Any,
    encoding: BufferEncoding,
    callback: js.Function1[/* error */ js.UndefOr[js.Error | Null], Unit]
  ): Unit = js.native
  
  var _writev: js.UndefOr[
    js.Function2[
      /* chunks */ js.Array[Chunk], 
      /* callback */ js.Function1[/* error */ js.UndefOr[js.Error | Null], Unit], 
      Unit
    ]
  ] = js.native
  
  def addListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  def addListener(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  /**
    * Event emitter
    * The defined events on documents including:
    * 1. close
    * 2. drain
    * 3. error
    * 4. finish
    * 5. pipe
    * 6. unpipe
    */
  @JSName("addListener")
  def addListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
  @JSName("addListener")
  def addListener_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
  @JSName("addListener")
  def addListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
  @JSName("addListener")
  def addListener_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
  @JSName("addListener")
  def addListener_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  @JSName("addListener")
  def addListener_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  
  /**
    * Is `true` after `'close'` has been emitted.
    * @since v18.0.0
    */
  val closed: Boolean = js.native
  
  /**
    * The `writable.cork()` method forces all written data to be buffered in memory.
    * The buffered data will be flushed when either the {@link uncork} or {@link end} methods are called.
    *
    * The primary intent of `writable.cork()` is to accommodate a situation in which
    * several small chunks are written to the stream in rapid succession. Instead of
    * immediately forwarding them to the underlying destination, `writable.cork()` buffers all the chunks until `writable.uncork()` is called, which will pass them
    * all to `writable._writev()`, if present. This prevents a head-of-line blocking
    * situation where data is being buffered while waiting for the first small chunk
    * to be processed. However, use of `writable.cork()` without implementing `writable._writev()` may have an adverse effect on throughput.
    *
    * See also: `writable.uncork()`, `writable._writev()`.
    * @since v0.11.2
    */
  def cork(): Unit = js.native
  
  /**
    * Destroy the stream. Optionally emit an `'error'` event, and emit a `'close'` event (unless `emitClose` is set to `false`). After this call, the writable
    * stream has ended and subsequent calls to `write()` or `end()` will result in
    * an `ERR_STREAM_DESTROYED` error.
    * This is a destructive and immediate way to destroy a stream. Previous calls to `write()` may not have drained, and may trigger an `ERR_STREAM_DESTROYED` error.
    * Use `end()` instead of destroy if data should flush before close, or wait for
    * the `'drain'` event before destroying the stream.
    *
    * Once `destroy()` has been called any further calls will be a no-op and no
    * further errors except from `_destroy()` may be emitted as `'error'`.
    *
    * Implementors should not override this method,
    * but instead implement `writable._destroy()`.
    * @since v8.0.0
    * @param error Optional, an error to emit with `'error'` event.
    */
  def destroy(): this.type = js.native
  def destroy(error: js.Error): this.type = js.native
  
  /**
    * Is `true` after `writable.destroy()` has been called.
    * @since v8.0.0
    */
  var destroyed: Boolean = js.native
  
  def emit(event: String, args: Any*): Boolean = js.native
  def emit(event: js.Symbol, args: Any*): Boolean = js.native
  @JSName("emit")
  def emit_close(event: close): Boolean = js.native
  @JSName("emit")
  def emit_drain(event: drain): Boolean = js.native
  @JSName("emit")
  def emit_error(event: error, err: js.Error): Boolean = js.native
  @JSName("emit")
  def emit_finish(event: finish): Boolean = js.native
  @JSName("emit")
  def emit_pipe(event: pipe, src: Readable): Boolean = js.native
  @JSName("emit")
  def emit_unpipe(event: unpipe, src: Readable): Boolean = js.native
  
  /**
    * Calling the `writable.end()` method signals that no more data will be written
    * to the `Writable`. The optional `chunk` and `encoding` arguments allow one
    * final additional chunk of data to be written immediately before closing the
    * stream.
    *
    * Calling the {@link write} method after calling {@link end} will raise an error.
    *
    * ```js
    * // Write 'hello, ' and then end with 'world!'.
    * const fs = require('node:fs');
    * const file = fs.createWriteStream('example.txt');
    * file.write('hello, ');
    * file.end('world!');
    * // Writing more now is not allowed!
    * ```
    * @since v0.9.4
    * @param chunk Optional data to write. For streams not operating in object mode, `chunk` must be a {string}, {Buffer},
    * {TypedArray} or {DataView}. For object mode streams, `chunk` may be any JavaScript value other than `null`.
    * @param encoding The encoding if `chunk` is a string
    * @param callback Callback for when the stream is finished.
    */
  def end(): this.type = js.native
  def end(cb: js.Function0[Unit]): this.type = js.native
  def end(chunk: Any): this.type = js.native
  def end(chunk: Any, cb: js.Function0[Unit]): this.type = js.native
  def end(chunk: Any, encoding: BufferEncoding): this.type = js.native
  def end(chunk: Any, encoding: BufferEncoding, cb: js.Function0[Unit]): this.type = js.native
  
  /**
    * Returns error if the stream has been destroyed with an error.
    * @since v18.0.0
    */
  val errored: js.Error | Null = js.native
  
  def on(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  def on(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  @JSName("on")
  def on_close(event: close, listener: js.Function0[Unit]): this.type = js.native
  @JSName("on")
  def on_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
  @JSName("on")
  def on_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
  @JSName("on")
  def on_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
  @JSName("on")
  def on_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  @JSName("on")
  def on_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  
  def once(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  def once(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  @JSName("once")
  def once_close(event: close, listener: js.Function0[Unit]): this.type = js.native
  @JSName("once")
  def once_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
  @JSName("once")
  def once_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
  @JSName("once")
  def once_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
  @JSName("once")
  def once_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  @JSName("once")
  def once_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  
  def prependListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  def prependListener(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  @JSName("prependListener")
  def prependListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
  @JSName("prependListener")
  def prependListener_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
  @JSName("prependListener")
  def prependListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
  @JSName("prependListener")
  def prependListener_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
  @JSName("prependListener")
  def prependListener_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  @JSName("prependListener")
  def prependListener_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  
  def prependOnceListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  def prependOnceListener(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  @JSName("prependOnceListener")
  def prependOnceListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
  @JSName("prependOnceListener")
  def prependOnceListener_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
  @JSName("prependOnceListener")
  def prependOnceListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
  @JSName("prependOnceListener")
  def prependOnceListener_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
  @JSName("prependOnceListener")
  def prependOnceListener_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  @JSName("prependOnceListener")
  def prependOnceListener_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  
  def removeListener(event: String, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  def removeListener(event: js.Symbol, listener: js.Function1[/* repeated */ Any, Unit]): this.type = js.native
  @JSName("removeListener")
  def removeListener_close(event: close, listener: js.Function0[Unit]): this.type = js.native
  @JSName("removeListener")
  def removeListener_drain(event: drain, listener: js.Function0[Unit]): this.type = js.native
  @JSName("removeListener")
  def removeListener_error(event: error, listener: js.Function1[/* err */ js.Error, Unit]): this.type = js.native
  @JSName("removeListener")
  def removeListener_finish(event: finish, listener: js.Function0[Unit]): this.type = js.native
  @JSName("removeListener")
  def removeListener_pipe(event: pipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  @JSName("removeListener")
  def removeListener_unpipe(event: unpipe, listener: js.Function1[/* src */ Readable, Unit]): this.type = js.native
  
  /**
    * The `writable.setDefaultEncoding()` method sets the default `encoding` for a `Writable` stream.
    * @since v0.11.15
    * @param encoding The new default encoding
    */
  def setDefaultEncoding(encoding: BufferEncoding): this.type = js.native
  
  /**
    * The `writable.uncork()` method flushes all data buffered since {@link cork} was called.
    *
    * When using `writable.cork()` and `writable.uncork()` to manage the buffering
    * of writes to a stream, defer calls to `writable.uncork()` using `process.nextTick()`. Doing so allows batching of all `writable.write()` calls that occur within a given Node.js event
    * loop phase.
    *
    * ```js
    * stream.cork();
    * stream.write('some ');
    * stream.write('data ');
    * process.nextTick(() => stream.uncork());
    * ```
    *
    * If the `writable.cork()` method is called multiple times on a stream, the
    * same number of calls to `writable.uncork()` must be called to flush the buffered
    * data.
    *
    * ```js
    * stream.cork();
    * stream.write('some ');
    * stream.cork();
    * stream.write('data ');
    * process.nextTick(() => {
    *   stream.uncork();
    *   // The data will not be flushed until uncork() is called a second time.
    *   stream.uncork();
    * });
    * ```
    *
    * See also: `writable.cork()`.
    * @since v0.11.2
    */
  def uncork(): Unit = js.native
  
  /**
    * Is `true` if it is safe to call `writable.write()`, which means
    * the stream has not been destroyed, errored, or ended.
    * @since v11.4.0
    */
  val writable: Boolean = js.native
  
  /**
    * Number of times `writable.uncork()` needs to be
    * called in order to fully uncork the stream.
    * @since v13.2.0, v12.16.0
    */
  val writableCorked: Double = js.native
  
  /**
    * Is `true` after `writable.end()` has been called. This property
    * does not indicate whether the data has been flushed, for this use `writable.writableFinished` instead.
    * @since v12.9.0
    */
  val writableEnded: Boolean = js.native
  
  /**
    * Is set to `true` immediately before the `'finish'` event is emitted.
    * @since v12.6.0
    */
  val writableFinished: Boolean = js.native
  
  /**
    * Return the value of `highWaterMark` passed when creating this `Writable`.
    * @since v9.3.0
    */
  val writableHighWaterMark: Double = js.native
  
  /**
    * This property contains the number of bytes (or objects) in the queue
    * ready to be written. The value provides introspection data regarding
    * the status of the `highWaterMark`.
    * @since v9.4.0
    */
  val writableLength: Double = js.native
  
  /**
    * Is `true` if the stream's buffer has been full and stream will emit `'drain'`.
    * @since v15.2.0, v14.17.0
    */
  val writableNeedDrain: Boolean = js.native
  
  /**
    * Getter for the property `objectMode` of a given `Writable` stream.
    * @since v12.3.0
    */
  val writableObjectMode: Boolean = js.native
  
  /**
    * The `writable.write()` method writes some data to the stream, and calls the
    * supplied `callback` once the data has been fully handled. If an error
    * occurs, the `callback` will be called with the error as its
    * first argument. The `callback` is called asynchronously and before `'error'` is
    * emitted.
    *
    * The return value is `true` if the internal buffer is less than the `highWaterMark` configured when the stream was created after admitting `chunk`.
    * If `false` is returned, further attempts to write data to the stream should
    * stop until the `'drain'` event is emitted.
    *
    * While a stream is not draining, calls to `write()` will buffer `chunk`, and
    * return false. Once all currently buffered chunks are drained (accepted for
    * delivery by the operating system), the `'drain'` event will be emitted.
    * Once `write()` returns false, do not write more chunks
    * until the `'drain'` event is emitted. While calling `write()` on a stream that
    * is not draining is allowed, Node.js will buffer all written chunks until
    * maximum memory usage occurs, at which point it will abort unconditionally.
    * Even before it aborts, high memory usage will cause poor garbage collector
    * performance and high RSS (which is not typically released back to the system,
    * even after the memory is no longer required). Since TCP sockets may never
    * drain if the remote peer does not read the data, writing a socket that is
    * not draining may lead to a remotely exploitable vulnerability.
    *
    * Writing data while the stream is not draining is particularly
    * problematic for a `Transform`, because the `Transform` streams are paused
    * by default until they are piped or a `'data'` or `'readable'` event handler
    * is added.
    *
    * If the data to be written can be generated or fetched on demand, it is
    * recommended to encapsulate the logic into a `Readable` and use {@link pipe}. However, if calling `write()` is preferred, it is
    * possible to respect backpressure and avoid memory issues using the `'drain'` event:
    *
    * ```js
    * function write(data, cb) {
    *   if (!stream.write(data)) {
    *     stream.once('drain', cb);
    *   } else {
    *     process.nextTick(cb);
    *   }
    * }
    *
    * // Wait for cb to be called before doing any other write.
    * write('hello', () => {
    *   console.log('Write completed, do more writes now.');
    * });
    * ```
    *
    * A `Writable` stream in object mode will always ignore the `encoding` argument.
    * @since v0.9.4
    * @param chunk Optional data to write. For streams not operating in object mode, `chunk` must be a {string}, {Buffer},
    * {TypedArray} or {DataView}. For object mode streams, `chunk` may be any JavaScript value other than `null`.
    * @param [encoding='utf8'] The encoding, if `chunk` is a string.
    * @param callback Callback for when this chunk of data is flushed.
    * @return `false` if the stream wishes for the calling code to wait for the `'drain'` event to be emitted before continuing to write additional data; otherwise `true`.
    */
  def write(chunk: Any): Boolean = js.native
  def write(chunk: Any, callback: js.Function1[/* error */ js.UndefOr[js.Error | Null], Unit]): Boolean = js.native
  def write(chunk: Any, encoding: BufferEncoding): Boolean = js.native
  def write(
    chunk: Any,
    encoding: BufferEncoding,
    callback: js.Function1[/* error */ js.UndefOr[js.Error | Null], Unit]
  ): Boolean = js.native
}