package typings.node.fsMod

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait ReadStreamOptions
  extends StObject
     with StreamOptions {
  
  var end: js.UndefOr[Double] = js.undefined
  
  var fs: js.UndefOr[CreateReadStreamFSImplementation | Null] = js.undefined
}
object ReadStreamOptions {
  
  inline def apply(): ReadStreamOptions = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[ReadStreamOptions]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: ReadStreamOptions] (val x: Self) extends AnyVal {
    
    inline def setEnd(value: Double): Self = StObject.set(x, "end", value.asInstanceOf[js.Any])
    
    inline def setEndUndefined: Self = StObject.set(x, "end", js.undefined)
    
    inline def setFs(value: CreateReadStreamFSImplementation): Self = StObject.set(x, "fs", value.asInstanceOf[js.Any])
    
    inline def setFsNull: Self = StObject.set(x, "fs", null)
    
    inline def setFsUndefined: Self = StObject.set(x, "fs", js.undefined)
  }
}