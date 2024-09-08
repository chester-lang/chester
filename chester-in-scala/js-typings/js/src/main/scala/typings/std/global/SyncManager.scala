package typings.std.global

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/* This class was inferred from a value with a constructor. In rare cases (like HTMLElement in the DOM) it might not work as you expect. */
@JSGlobal("SyncManager")
@js.native
/* standard dom */
open class SyncManager ()
  extends StObject
     with typings.std.SyncManager {
  
  /* standard dom */
  /* CompleteClass */
  override def getTags(): js.Promise[js.Array[java.lang.String]] = js.native
  
  /* standard dom */
  /* CompleteClass */
  override def register(tag: java.lang.String): js.Promise[Unit] = js.native
}