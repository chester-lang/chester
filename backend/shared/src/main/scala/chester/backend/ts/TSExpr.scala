package chester.backend.ts

import chester.i18n.d
import chester.syntax.{Tree, TreeMap}
import chester.utils.doc.*
import upickle.default.*

sealed trait TSExpr extends Tree[TSExpr] with ToDoc derives ReadWriter {
  def meta: Option[Meta]
}

case class Void0Expr(meta: Option[Meta] = None) extends TSExpr {
  override type ThisTree = Void0Expr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): Void0Expr = this

  override def toDoc(using DocConf): Doc = Doc.text("(void 0)")
}

case class IdentifierExpr(name: String, meta: Option[Meta] = None) extends TSExpr {
  override type ThisTree = IdentifierExpr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): IdentifierExpr = this

  override def toDoc(using DocConf): Doc = Doc.text(name)
}

case class RawExpr(code: String, meta: Option[Meta] = None) extends TSExpr {
  override type ThisTree = RawExpr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): RawExpr = this

  override def toDoc(using DocConf): Doc = "(" + code + ")"
}

case class DoubleExpr(value: Double, meta: Option[Meta] = None) extends TSExpr {
  override type ThisTree = DoubleExpr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): DoubleExpr = this

  override def toDoc(using DocConf): Doc = if (value.isValidInt) {
    value.toInt.toString
  } else {
    value.toString
  }
}

case class LambdaExpr(
    params: Seq[Param],
    body: TSExpr,
    meta: Option[Meta] = None
) extends TSExpr {
  override type ThisTree = LambdaExpr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): LambdaExpr = copy(
    params = params.map(g(_)),
    body = f(body)
  )

  override def toDoc(using DocConf): Doc = {
    val paramsDoc = Doc.mkList(params, begin = "(", end = ")")
    d"function $paramsDoc { return ${body.toDoc}; }"
  }
}

case class FunctionCallExpr(
    callee: TSExpr,
    args: Seq[TSExpr],
    meta: Option[Meta] = None
) extends TSExpr {
  override type ThisTree = FunctionCallExpr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): FunctionCallExpr = copy(
    callee = f(callee),
    args = args.map(g(_))
  )

  override def toDoc(using DocConf): Doc = {
    val argsDoc = Doc.mkList(args, begin = "(", end = ")")
    callee.toDoc <> argsDoc
  }
}

sealed trait TSStmt extends TSExpr derives ReadWriter {
  override type ThisTree <: TSStmt
}

case class EmptyStmt(meta: Option[Meta] = None) extends TSStmt {
  override type ThisTree = EmptyStmt

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): EmptyStmt = this

  override def toDoc(using DocConf): Doc = ";"
}

case class ConstStmt(name: String, ty: Option[TSType], value: TSExpr, meta: Option[Meta] = None) extends TSStmt {

  override type ThisTree = ConstStmt

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): ConstStmt = copy(
    ty = ty.map(g(_)),
    value = f(value)
  )

  override def toDoc(using DocConf): Doc = ty match {
    case Some(tyExpr) =>
      "const" <+> name <>
        ":" <+> tyExpr <+>
        "=" <+> value <> ";"
    case None =>
      "const" <+> name <+>
        "=" <+> value <> ";"
  }
}

sealed trait TSType extends TSExpr derives ReadWriter {
  override type ThisTree <: TSType
}

case class NumberType(meta: Option[Meta] = None) extends TSType {
  override def toDoc(using DocConf): Doc = "number"

  override type ThisTree = NumberType

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): NumberType = this
}

case class NeverType(meta: Option[Meta] = None) extends TSType {
  override def toDoc(using DocConf): Doc = "never"

  override type ThisTree = NeverType

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): NeverType = this
}

case class Param(name: String, ty: TSType, meta: Option[Meta] = None) extends TSExpr {
  override type ThisTree = Param

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): Param = copy(
    ty = g(ty)
  )

  override def toDoc(using DocConf): Doc = name <> ":" <+> ty.toDoc
}

case class TSFunctionType(
    params: Seq[Param],
    result: TSType,
    meta: Option[Meta] = None
) extends TSType {
  override type ThisTree = TSFunctionType
  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): TSFunctionType = copy(
    params = params.map(g(_)),
    result = g(result)
  )
  override def toDoc(using DocConf): Doc = {
    val paramsDoc = Doc.mkList(params.map(_.toDoc))
    val resultDoc = result.toDoc
    d"($paramsDoc) => $resultDoc"
  }
}
