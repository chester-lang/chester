package chester.utils.doc

object Docs {
  def `{`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("begin ") else Doc.text("{")
  }
  def `}`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("end ") else Doc.text("}")
  }
  def `[`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("begin list ") else Doc.text("[")
  }
  def `]`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("end list ") else Doc.text("]")
  }
  def `(`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("beginTuple ") else Doc.text("(")
  }
  def `)`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("endTuple ") else Doc.text(")")
  }
  def `->`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("to ") else Doc.text("->")
  }
  def `:`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("is ") else Doc.text(":")
  }
  def `...`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("ellipsis ") else Doc.text("...")
  }
  def `=`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("equals ") else Doc.text("=")
  }
  def `,`(using DocConf): Doc = Doc.text(",")
  def `/`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("slash ") else Doc.text("/")
  }
  def `=>`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("returns ") else Doc.text("=>")
  }
  def `.`(using DocConf): Doc = Doc.text(".")
  def `;`(using DocConf): Doc = Doc.text(";")
  def `<:`(using DocConf): Doc = {
    if (ReplaceBracketsWithWord.get) Doc.text("isSubtypeOf ") else Doc.text("<:")
  }
  val `with`: Doc = Doc.text("with")

  def parens(doc: Doc)(using DocConf): Doc = `(` <> doc <> `)`
  def brackets(doc: Doc)(using DocConf): Doc = `[` <> doc <> `]`
  def braces(doc: Doc)(using DocConf): Doc = `{` <> doc <> `}`
}
