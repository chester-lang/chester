package chester.elab

import chester.syntax.Name
import chester.syntax.core.*

object PreludeBuiltin {
  case class BuiltinItem(id: Name, value: Term, ty: Term) {}

  val builtinItems: Seq[BuiltinItem] = Vector(
    BuiltinItem("Type", Type0, Type0),
    BuiltinItem("Int", IntType(None), Type0),
    BuiltinItem("Integer", IntegerType(None), Type0),
    BuiltinItem("Float", FloatType(None), Type0),
    BuiltinItem("Rational", RationalType(None), Type0),
    BuiltinItem("String", StringType(None), Type0),
    BuiltinItem("Symbol", SymbolType(None), Type0),
    BuiltinItem("List", ListF(None), TyToty)
  )

}
