package typings.node.fsMod

import typings.node.nodeBooleans.`true`
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait BigIntOptions extends StObject {
  
  var bigint: `true`
}
object BigIntOptions {
  
  inline def apply(): BigIntOptions = {
    val __obj = js.Dynamic.literal(bigint = true)
    __obj.asInstanceOf[BigIntOptions]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: BigIntOptions] (val x: Self) extends AnyVal {
    
    inline def setBigint(value: `true`): Self = StObject.set(x, "bigint", value.asInstanceOf[js.Any])
  }
}
