package typings.node.anon

import typings.node.bufferMod.global.BufferEncoding
import typings.node.fsMod.OpenMode
import typings.node.globalsMod.global.AbortSignal
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/* Inlined node.node:fs.ObjectEncodingOptions & {  mode :node.node:fs.Mode | undefined,   flag :node.node:fs.OpenMode | undefined,   flush :boolean | undefined} & node.node:events.Abortable */
trait ObjectEncodingOptionsmode extends StObject {
  
  var encoding: js.UndefOr[BufferEncoding | Null] = js.undefined
  
  var flag: js.UndefOr[OpenMode] = js.undefined
  
  /**
    * If all data is successfully written to the file, and `flush`
    * is `true`, `filehandle.sync()` is used to flush the data.
    * @default false
    */
  var flush: js.UndefOr[Boolean] = js.undefined
  
  var mode: js.UndefOr[typings.node.fsMod.Mode] = js.undefined
  
  /**
    * When provided the corresponding `AbortController` can be used to cancel an asynchronous action.
    */
  var signal: js.UndefOr[AbortSignal] = js.undefined
}
object ObjectEncodingOptionsmode {
  
  inline def apply(): ObjectEncodingOptionsmode = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[ObjectEncodingOptionsmode]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: ObjectEncodingOptionsmode] (val x: Self) extends AnyVal {
    
    inline def setEncoding(value: BufferEncoding): Self = StObject.set(x, "encoding", value.asInstanceOf[js.Any])
    
    inline def setEncodingNull: Self = StObject.set(x, "encoding", null)
    
    inline def setEncodingUndefined: Self = StObject.set(x, "encoding", js.undefined)
    
    inline def setFlag(value: OpenMode): Self = StObject.set(x, "flag", value.asInstanceOf[js.Any])
    
    inline def setFlagUndefined: Self = StObject.set(x, "flag", js.undefined)
    
    inline def setFlush(value: Boolean): Self = StObject.set(x, "flush", value.asInstanceOf[js.Any])
    
    inline def setFlushUndefined: Self = StObject.set(x, "flush", js.undefined)
    
    inline def setMode(value: typings.node.fsMod.Mode): Self = StObject.set(x, "mode", value.asInstanceOf[js.Any])
    
    inline def setModeUndefined: Self = StObject.set(x, "mode", js.undefined)
    
    inline def setSignal(value: AbortSignal): Self = StObject.set(x, "signal", value.asInstanceOf[js.Any])
    
    inline def setSignalUndefined: Self = StObject.set(x, "signal", js.undefined)
  }
}
