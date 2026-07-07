package chester

object Formatter {

  def toDoc(cst: CST)(using conf: DocConf): Doc = cst match
    case CST.Symbol(name, _) =>
      text(name)
    case CST.Tuple(elements, _) =>
      if elements.isEmpty then text("()")
      else
        val inner = elements.map(_.toDoc).reduce((a, b) => a <> text(", ") <> b)
        text("(") <> inner <> text(")")
    case CST.ListLiteral(elements, _) =>
      if elements.isEmpty then text("[]")
      else
        val inner = elements.map(_.toDoc).reduce((a, b) => a <> text(", ") <> b)
        text("[") <> inner <> text("]")
    case CST.Block(elements, tail, _) =>
      val stmts = elements.map(e => e.toDoc <> text(";"))
      val t = tail.map(_.toDoc).getOrElse(empty)
      val all = if t == empty then stmts else stmts :+ t
      if all.isEmpty then text("{}")
      else
        val inner = all.reduce((a, b) => a <@> b)
        text("{") <@> inner.indented() <@> text("}")
    case CST.StringLiteral(value, _) =>
      text("\"" + value.replace("\"", "\\\"").replace("\n", "\\n") + "\"")
    case CST.IntegerLiteral(value, _) =>
      text(value.toString)
    case CST.SeqOf(elements, _) =>
      elements.toVector.map(_.toDoc).reduce((a, b) => a <+> b)
    case CST.Comment(txt, kind, _) =>
      kind match
        case CommentKind.Line => text("//" + txt)
        case CommentKind.Block => text("/*" + txt + "*/")

  def format(cst: CST): String = {
    given conf: DocConf = DocConf.Default
    toDoc(cst).layout(0)
  }
}
