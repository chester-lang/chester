package typings.node.anon

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/* Inlined node.node:fs.MakeDirectoryOptions & {  recursive :false | undefined} */
trait MakeDirectoryOptionsrecurMode extends StObject {
  
  /**
    * A file mode. If a string is passed, it is parsed as an octal integer. If not specified
    * @default 0o777
    */
  var mode: js.UndefOr[typings.node.fsMod.Mode] = js.undefined
  
  /**
    * Indicates whether parent folders should be created.
    * If a folder was created, the path to the first created folder will be returned.
    * @default false
    */
  var recursive: js.UndefOr[Boolean] = js.undefined
}
object MakeDirectoryOptionsrecurMode {
  
  inline def apply(): MakeDirectoryOptionsrecurMode = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[MakeDirectoryOptionsrecurMode]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: MakeDirectoryOptionsrecurMode] (val x: Self) extends AnyVal {
    
    inline def setMode(value: typings.node.fsMod.Mode): Self = StObject.set(x, "mode", value.asInstanceOf[js.Any])
    
    inline def setModeUndefined: Self = StObject.set(x, "mode", js.undefined)
    
    inline def setRecursive(value: Boolean): Self = StObject.set(x, "recursive", value.asInstanceOf[js.Any])
    
    inline def setRecursiveUndefined: Self = StObject.set(x, "recursive", js.undefined)
  }
}
