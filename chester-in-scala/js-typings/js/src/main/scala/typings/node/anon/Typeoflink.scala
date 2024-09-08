package typings.node.anon

import typings.node.fsMod.NoParamCallback
import typings.node.fsMod.PathLike
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

@js.native
trait Typeoflink extends StObject {
  
  def apply(existingPath: PathLike, newPath: PathLike, callback: NoParamCallback): Unit = js.native
  
  /**
    * Asynchronous link(2) - Create a new link (also known as a hard link) to an existing file.
    * @param existingPath A path to a file. If a URL is provided, it must use the `file:` protocol.
    * @param newPath A path to a file. If a URL is provided, it must use the `file:` protocol.
    */
  def __promisify__(existingPath: PathLike, newPath: PathLike): js.Promise[Unit] = js.native
}