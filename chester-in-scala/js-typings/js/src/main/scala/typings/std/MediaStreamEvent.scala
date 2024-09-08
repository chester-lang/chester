package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/** Events that occurs in relation to a MediaStream. Two events of this type can be thrown: addstream and removestream. */
@js.native
trait MediaStreamEvent
  extends StObject
     with Event {
  
  /* standard dom */
  val stream: org.scalajs.dom.MediaStream | Null = js.native
}