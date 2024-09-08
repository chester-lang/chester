package typings.node.anon

import typings.node.bufferMod.global.BufferEncoding
import typings.node.globalsMod.global.AbortSignal
import typings.node.nodeStrings.buffer_
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/* Inlined node.node:fs.WatchOptions & {  encoding :'buffer'} */
trait WatchOptionsencodingbuffe extends StObject {
  
  var encoding: js.UndefOr[BufferEncoding | buffer_] = js.undefined
  
  var persistent: js.UndefOr[Boolean] = js.undefined
  
  var recursive: js.UndefOr[Boolean] = js.undefined
  
  /**
    * When provided the corresponding `AbortController` can be used to cancel an asynchronous action.
    */
  var signal: js.UndefOr[AbortSignal] = js.undefined
}
object WatchOptionsencodingbuffe {
  
  inline def apply(): WatchOptionsencodingbuffe = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[WatchOptionsencodingbuffe]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: WatchOptionsencodingbuffe] (val x: Self) extends AnyVal {
    
    inline def setEncoding(value: BufferEncoding | buffer_): Self = StObject.set(x, "encoding", value.asInstanceOf[js.Any])
    
    inline def setEncodingUndefined: Self = StObject.set(x, "encoding", js.undefined)
    
    inline def setPersistent(value: Boolean): Self = StObject.set(x, "persistent", value.asInstanceOf[js.Any])
    
    inline def setPersistentUndefined: Self = StObject.set(x, "persistent", js.undefined)
    
    inline def setRecursive(value: Boolean): Self = StObject.set(x, "recursive", value.asInstanceOf[js.Any])
    
    inline def setRecursiveUndefined: Self = StObject.set(x, "recursive", js.undefined)
    
    inline def setSignal(value: AbortSignal): Self = StObject.set(x, "signal", value.asInstanceOf[js.Any])
    
    inline def setSignalUndefined: Self = StObject.set(x, "signal", js.undefined)
  }
}
