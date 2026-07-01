package chester.utils

package object doc {
  type Doc = chester.Doc
  val Doc = chester.Doc
  type ToDoc = chester.ToDoc
  type DocConf = chester.DocConf
  val DocConf = chester.DocConf

  inline def empty: Doc = chester.empty
  inline def hardline: Doc = chester.hardline
  inline def text(s: String): Doc = chester.text(s)
  inline def hsep(docs: Seq[Doc], sep: Doc): Doc = chester.hsep(docs, sep)
  inline def ssep(docs: Seq[Doc], sep: Doc): Doc = chester.ssep(docs, sep)
  inline def concat(docs: Seq[Doc]): Doc = chester.concat(docs)
  inline def render(doc: Doc): Doc = doc
  
  val line = chester.hardline

  implicit class DocOps(val doc: Doc) extends AnyVal {
    def render(w: Int = 80): String = doc.layout(0)
  }

  object StringPrinter {
    def render(doc: ToDoc)(using DocConf): String = doc.toDoc.layout(0)
  }

  extension (left: ToDoc) {
    inline def <>(right: ToDoc): Doc = chester.<>(left.toDoc)(right.toDoc)
    inline def <+>(right: ToDoc): Doc = chester.<+>(left.toDoc)(right.toDoc)
    inline def <@>(right: ToDoc): Doc = chester.<@>(left.toDoc)(right.toDoc)
    inline def <@@>(right: ToDoc): Doc = chester.<@@>(left.toDoc)(right.toDoc)
    inline def indented(): Doc = chester.indented(left.toDoc)()
    inline def </>(right: ToDoc): Doc = chester.<@>(left.toDoc)(right.toDoc)
    inline def <|>(right: ToDoc): Doc = chester.<@>(left.toDoc)(right.toDoc)
    inline def end: Doc = left.toDoc <> chester.hardline
  }
}
