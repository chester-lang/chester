package chester.i18n

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
  def stringContextToString(sc: StringContext): String = {

    val stringbuilder = new StringBuilder()
    val parts = sc.parts.iterator
    var index = 1
    if parts.hasNext then
      stringbuilder.append(parts.next().replace("$", "$$"))
    while parts.hasNext do
      stringbuilder.append(s"$$$index")
      stringbuilder.append(parts.next().replace("$", "$$"))
      index += 1
    stringbuilder.result()
  }

  def applyTemplate(template: String, args: Vector[Any]): String = {
    if (args.length > 9)
      throw new IllegalArgumentException("Too many arguments")
    var result = template
    val xs = args.map(_.toString)
    for (i <- xs.indices) {
      val newResult = result.replace(s"$$${i + 1}", xs(i))
      if (newResult == result)
        throw new IllegalArgumentException(
          s"Missing argument ${i + 1} in template $template"
        )
      result = newResult
    }
    for (i <- 1 to 9) {
      for (x <- xs)
        if (x.contains("s$$${i}"))
          throw new IllegalArgumentException(s"Unexpected $i in args $args")
      if (result.contains("s$$${i}"))
        throw new IllegalArgumentException(s"Missing argument $i in args $args")
    }
    result.replace("$$", "$")
  }
}
