package typings.undiciTypes.mod

import org.scalajs.dom.URL
import typings.undiciTypes.poolMod.Pool.Options
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

@JSImport("undici-types", "BalancedPool")
@js.native
open class BalancedPool protected ()
  extends typings.undiciTypes.balancedPoolMod.default {
  def this(url: String) = this()
  def this(url: js.Array[String | URL]) = this()
  def this(url: URL) = this()
  def this(url: String, options: Options) = this()
  def this(url: js.Array[String | URL], options: Options) = this()
  def this(url: URL, options: Options) = this()
}
