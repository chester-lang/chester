package chester.i18n

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.language.implicitConversions
import scala.quoted.*
import scala.util.control.NonFatal
import upickle.default.*

trait T {
  def t(args: Any*): String
}

private object TranslationRepository:
  private var cachedTranslations: Option[Map[String, Map[String, String]]] = None
  private var cachedFileSnapshot: Option[String] = None
  private val defaultLanguage = Language(LanguageTag.EN)

  def translationTemplate(scExpr: Expr[StringContext])(using Quotes): String =
    val sc = scExpr match
      case '{ StringContext(${Varargs(parts)}*) } =>
        val evaluated = parts.map(_.valueOrAbort)
        new StringContext(evaluated*)
      case _ =>
        quotes.reflect.report.error("t interpolator only supports literal string contexts")
        new StringContext("")
    val template = Template.stringContextToString(sc)
    val resolved = resolveTemplate(template)
    resolved

  private def resolveTemplate(template: String)(using Quotes): String =
    val lang = detectLanguage()
    val locales = Seq(
      s"${lang.tag.name}_${lang.region.name}",
      lang.tag.name
    ).map(normalizeLocaleKey).distinct
    val translations = loadTranslations()
    locales
      .iterator
      .flatMap(locale => translations.get(locale).flatMap(_.get(template)))
      .nextOption()
      .getOrElse(template)

  private def detectLanguage(): Language =
    val env = sys.env
    val rawLocale = env
      .get("CHESTER_I18N_LOCALE")
      .orElse(env.get("LC_ALL"))
      .orElse(env.get("LANG"))
      .orElse(env.get("LANGUAGE"))
    rawLocale.flatMap(parseLanguage).getOrElse(defaultLanguage)

  private def parseLanguage(raw: String): Option[Language] =
    val sanitized = raw
      .takeWhile(ch => ch != '.' && ch != '@')
      .replace('-', '_')
    val parts = sanitized.split("_").filter(_.nonEmpty)
    parts match
      case Array(languagePart, regionPart, _*) =>
        for
          tag <- LanguageTag.values.find(_.is(languagePart))
          region <- RegionTag.values.find(_.is(regionPart))
        yield Language(tag, region)
      case Array(languagePart) =>
        LanguageTag.values.find(_.is(languagePart)).map(Language(_))
      case _ => None

  private def normalizeLocaleKey(raw: String): String =
    val sanitized = raw
      .takeWhile(ch => ch != '.' && ch != '@')
      .replace('-', '_')
    val parts = sanitized.split("_").filter(_.nonEmpty)
    parts match
      case Array(languagePart, regionPart, _*) =>
        s"${languagePart.toLowerCase}_${regionPart.toUpperCase}"
      case Array(languagePart) =>
        languagePart.toLowerCase
      case _ => raw.toLowerCase

  private def loadTranslations()(using Quotes): Map[String, Map[String, String]] =
    val path = translationFile
    val content =
      try Files.readString(path)
      catch case NonFatal(_) => "{}"

    if cachedFileSnapshot.contains(content) then
      cachedTranslations.getOrElse(Map.empty)
    else
      val parsed = parseTranslationContent(content)
      cachedFileSnapshot = Some(content)
      cachedTranslations = Some(parsed)
      parsed

  private def parseTranslationContent(content: String): Map[String, Map[String, String]] =
    try
      val json = ujson.read(content)
      json.obj.view.flatMap { case (locale, value) =>
        value.objOpt.map { inner =>
          val entries = inner.view.collect { case (key, ujson.Str(strValue)) => key -> strValue }.toMap
          normalizeLocaleKey(locale) -> entries
        }
      }.toMap
    catch
      case NonFatal(_) => Map.empty

  private def translationFile: Path =
    val envPath = sys.env.get("CHESTER_I18N_FILE").map(Paths.get(_))
    val defaultPath = Paths.get("i18n", "translations.json")
    val target = envPath.getOrElse(defaultPath)
    if Files.notExists(target) then
      val parent = target.getParent
      if parent != null then Files.createDirectories(parent)
      Files.writeString(target, "{}\n", StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      cachedFileSnapshot = Some("{}\n")
      cachedTranslations = Some(Map.empty)
    target

  def ensureTemplateRegistered(template: String)(using Quotes): Unit =
    val translations = loadTranslations()
    val missingLocales = LanguageTag.values.map(localeKeyFor).filter { locale =>
      translations.get(locale).forall(!_.contains(template))
    }
    if missingLocales.nonEmpty then
      val updates = missingLocales.map(locale => locale -> Map(template -> template)).toMap
      val updated = updateTranslationFile(updates)
      cachedFileSnapshot = Some(updated._1)
      cachedTranslations = Some(updated._2)

  private def updateTranslationFile(newEntries: Map[String, Map[String, String]]): (String, Map[String, Map[String, String]]) =
    val path = translationFile
    val currentContent =
      cachedFileSnapshot.orElse {
        try Some(Files.readString(path))
        catch case NonFatal(_) => None
      }.getOrElse("{}")
    val parsed = parseTranslationContent(currentContent)
    val updatedAll = newEntries.foldLeft(parsed) { case (acc, (locale, entries)) =>
      val localeMap = acc.getOrElse(locale, Map.empty)
      acc + (locale -> (localeMap ++ entries))
    }

    val jsonObj = ujson.Obj()
    updatedAll.toSeq.sortBy(_._1).foreach { case (loc, entries) =>
      val inner = ujson.Obj()
      entries.toSeq.sortBy(_._1).foreach { case (entryKey, entryValue) =>
        inner(entryKey) = ujson.Str(entryValue)
      }
      jsonObj(loc) = inner
    }
    val rendered = jsonObj.render(2) + "\n"
    Files.writeString(path, rendered, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    (rendered, updatedAll)

  private def localeKeyFor(tag: LanguageTag): String =
    normalizeLocaleKey(s"${tag.name}_${tag.defaultRegion.name}")

private def tMacro(sc: Expr[StringContext])(using Quotes): Expr[T] =
  val resolvedTemplate = TranslationRepository.translationTemplate(sc)
  TranslationRepository.ensureTemplateRegistered(resolvedTemplate)
  val templateExpr = Expr(resolvedTemplate)(using summon[ToExpr[String]])
  '{
    new T {
      def t(args: Any*): String =
        Template.applyTemplate($templateExpr, args.toVector)
    }
  }

implicit inline def t(inline sc: StringContext): T = ${ tMacro('sc) }
