package chester.i18n

import chester.utils.doc.{<>, Doc, DocConf, ToDoc}

case class Language(tag: LanguageTag, region: RegionTag) {
  def name: String = s"${tag.name}_${region.name}"

  override def toString: String = name
}

object Language {
  def apply(tag: LanguageTag): Language = new Language(tag, tag.defaultRegion)

  private val languages = LanguageTag.values
  private val regions = RegionTag.values

  private def fromOption(x: String): Option[Language] = {
    // split by _ or -
    val parts = x.split("[_\\-]")
    if (parts.length == 1) {
      languages.find(_.is(parts(0))).map(Language(_))
    } else if (parts.length == 2) {
      for {
        l <- languages.find(_.is(parts(0)))
        r <- regions.find(_.is(parts(1)))
      } yield Language(l, r)
    } else {
      None
    }
  }

  def from(x: String): Language = fromOption(x).getOrElse(
    throw new IllegalArgumentException(s"Invalid language $x")
  )
}

enum LanguageTag {
  case EN, ZH

  def name: String = toString.toLowerCase()

  def is(x: String): Boolean = x.toLowerCase() == name

  def defaultRegion: RegionTag = this match {
    case EN => RegionTag.NZ
    case ZH => RegionTag.TW
  }
}

enum RegionTag {
  case NZ, AU, TW, HK, US, BG

  def name: String = toString.toUpperCase()

  def is(x: String): Boolean = x.toUpperCase() == name
}

case class RegionTable(table: Map[RegionTag, Map[String, String]]) {
  private val alternatives: Vector[Map[String, String]] =
    table.toSeq.sortBy((_, map) => -map.size).map(_._2).toVector

  def get(region: RegionTag, context: String): String = {
    import scala.util.boundary
    boundary {
      table.get(region).flatMap(_.get(context)) match {
        case Some(value) => boundary.break(value)
        case None        =>
      }
      alternatives.foreach { map =>
        map.get(context) match {
          case Some(value) => boundary.break(value)
          case None        =>
        }
      }
      context
    }
  }
}

case class TranslationTable(table: Map[LanguageTag, RegionTable]) {
  def get(lang: Language, context: String): String =
    table.get(lang.tag).map(_.get(lang.region, context)).getOrElse(context)
}

object Template {
  sealed trait DocTemplatePart
  object DocTemplatePart {
    case class Literal(value: String) extends DocTemplatePart
    case class Placeholder(index: Int) extends DocTemplatePart
  }
  import DocTemplatePart.*

  def stringContextToString(sc: StringContext): String = {

    val stringbuilder = new StringBuilder()
    val parts = sc.parts.iterator
    var index = 1
    if parts.hasNext then stringbuilder.append(parts.next().replace("$", "$$"))
    while parts.hasNext do
      stringbuilder.append(s"$$$index")
      stringbuilder.append(parts.next().replace("$", "$$"))
      index += 1
    stringbuilder.result()
  }

  def applyTemplate(template: String, args: Vector[Any]): String = {
    if (args.length > 9) {
      throw new IllegalArgumentException("Too many arguments")
    }
    var result = template
    val xs = args.map(_.toString)
    for (i <- xs.indices) {
      val newResult = result.replace(s"$$${i + 1}", xs(i))
      if (newResult == result) {
        throw new IllegalArgumentException(
          s"Missing argument ${i + 1} in template $template"
        )
      }
      result = newResult
    }
    for (i <- 1 to 9) {
      for (x <- xs) {
        if (x.contains("s$$${i}")) {
          throw new IllegalArgumentException(s"Unexpected $i in args $args")
        }
      }
      if (result.contains("s$$${i}")) {
        throw new IllegalArgumentException(s"Missing argument $i in args $args")
      }
    }
    result.replace("$$", "$")
  }

  def renderDocFromStringContext(parts: Vector[String], args: Vector[ToDoc])(using DocConf): Doc = {
    if parts.length != args.length + 1 then
      throw new IllegalArgumentException(s"Invalid string context: ${parts.length} parts for ${args.length} args")
    val literalDocs =
      parts.iterator.map(part => if part.isEmpty then None else Some(Doc.text(part))).toVector
    val combined = Vector.newBuilder[Doc]
    var idx = 0
    while idx < args.length do
      literalDocs(idx).foreach(combined += _)
      combined += args(idx).toDoc
      idx += 1
    literalDocs.lastOption.flatten.foreach(combined += _)
    concatenateDocs(combined.result())
  }

  def docPartsFromTemplate(template: String): Vector[DocTemplatePart] = {
    val builder = Vector.newBuilder[DocTemplatePart]
    val literal = new StringBuilder
    var idx = 0
    def flushLiteral(): Unit = {
      if literal.nonEmpty then
        builder += Literal(literal.result())
        literal.clear()
    }
    while idx < template.length do
      val ch = template.charAt(idx)
      if ch == '$' && idx + 1 < template.length then
        template.charAt(idx + 1) match
          case '$' =>
            literal.append('$')
            idx += 2
          case d if d.isDigit =>
            flushLiteral()
            val placeholderIndex = d.asDigit
            builder += Placeholder(placeholderIndex)
            idx += 2
          case _ =>
            literal.append(ch)
            idx += 1
      else {
        literal.append(ch)
        idx += 1
      }
    flushLiteral()
    builder.result()
  }

  def renderDocFromTemplateParts(parts: Vector[DocTemplatePart], args: Vector[ToDoc])(using DocConf): Doc = {
    val docs = Vector.newBuilder[Doc]
    parts.foreach {
      case Literal(value) if value.nonEmpty =>
        docs += Doc.text(value)
      case Literal(_) => // drop empty literal
      case Placeholder(index) =>
        val zeroIdx = index - 1
        if zeroIdx < 0 || zeroIdx >= args.length then throw new IllegalArgumentException(s"Missing argument $index in template")
        docs += args(zeroIdx).toDoc
    }
    concatenateDocs(docs.result())
  }

  private def concatenateDocs(docs: Vector[Doc])(using DocConf): Doc = {
    docs.headOption match
      case Some(head) => docs.tail.foldLeft(head)((acc, doc) => acc <> doc)
      case None       => Doc.empty
  }
}
