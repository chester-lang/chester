package chester.utils.doc

import kiama2.output.ParenPrettyPrinter

trait FansiPrettyPrinter extends StylePrettyPrinter {
  type Layout = fansi.Str
  type Builder = fansi.Str
  override def newBuilder: Builder = ""
  override def BuilderAppend(builder: Builder, text: Text): Builder = {
    builder ++ fansi.Str(text.s).overlay(text.attr.toFansi)
  }
  override def BuilderResult(builder: Builder): Layout = builder
}
object FansiPrettyPrinter extends FansiPrettyPrinter with ParenPrettyPrinter

trait ColorfulPrettyPrinter extends StylePrettyPrinter {
  type Layout = Colorful
  type Builder = Colorful
  override def newBuilder: Builder = Colorful.Empty
  override def BuilderAppend(builder: Builder, text: Text): Builder = {
    builder :+ ColorfulPiece(text.s, text.attr)
  }
  override def BuilderResult(builder: Builder): Layout = builder
}

object ColorfulPrettyPrinter extends ColorfulPrettyPrinter with ParenPrettyPrinter
