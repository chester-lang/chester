package chester

import upickle.default.*

sealed trait Doc extends ToDoc derives ReadWriter {
  def layout(indent: Int): String
  override def toString: String = layout(0)
  override def toDoc(using DocConf): Doc = this
}

object Doc {
  case class Empty() extends Doc {
    def layout(indent: Int): String = ""
  }
  case class Text(s: String) extends Doc {
    def layout(indent: Int): String = s
  }
  case class Concat(left: Doc, right: Doc) extends Doc {
    def layout(indent: Int): String = left.layout(indent) + right.layout(indent)
  }
  case class SpaceConcat(left: Doc, right: Doc) extends Doc {
    def layout(indent: Int): String = {
      val l = left.layout(indent)
      val r = right.layout(indent)
      if (l.isEmpty) r
      else if (r.isEmpty) l
      else l + " " + r
    }
  }
  case class LineConcat(left: Doc, right: Doc) extends Doc {
    def layout(indent: Int): String = {
      val l = left.layout(indent)
      val r = right.layout(indent)
      if (l.isEmpty) r
      else if (r.isEmpty) l
      else l + "\n" + ("  " * indent) + r
    }
  }
  case class Indent(doc: Doc) extends Doc {
    def layout(indent: Int): String = doc.layout(indent + 1)
  }

  implicit def stringToDoc(s: String): Doc = Text(s)
}

trait ToDoc {
  def toDoc(using DocConf): Doc
}

case class DocConf()
object DocConf {
  given Default: DocConf = DocConf()
}

import Doc.*

val empty: Doc = Empty()
val hardline: Doc = Text("\n")

extension (left: Doc) {
  def <>(right: Doc): Doc = Concat(left, right)
  def <+>(right: Doc): Doc = SpaceConcat(left, right)
  def <@>(right: Doc): Doc = LineConcat(left, right)
  def <@@>(right: Doc): Doc = LineConcat(left, right)
  def indented(): Doc = Indent(left)
}

def text(s: String): Doc = Text(s)

def hsep(docs: Seq[Doc], sep: Doc): Doc = {
  if (docs.isEmpty) empty
  else docs.reduceLeft((acc, d) => Concat(Concat(acc, sep), d))
}

def ssep(docs: Seq[Doc], sep: Doc): Doc = {
  if (docs.isEmpty) empty
  else if (sep == hardline || sep.isInstanceOf[LineConcat]) docs.reduceLeft((acc, d) => LineConcat(acc, d))
  else docs.reduceLeft((acc, d) => Concat(Concat(acc, sep), d))
}

def concat(docs: Seq[Doc]): Doc = {
  if (docs.isEmpty) empty
  else docs.reduceLeft((acc, d) => Concat(acc, d))
}
