package chester.i18n

import chester.utils.Parameter
import chester.utils.doc.{Doc, DocConf, ToDoc}

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.language.implicitConversions
import scala.quoted.*
import scala.util.control.NonFatal

trait T {
  def t(args: Any*): String
}

/** Interpolator for Docs without translation; turns parts + args into a Doc directly. */
trait D:
  def d(args: ToDoc*)(using DocConf): Doc

/** Interpolator for translated Docs; resolves the string through i18n before building a Doc. */
trait DT:
  def dt(args: ToDoc*)(using DocConf): Doc

implicit inline def d(inline sc: StringContext): D = ${ dMacro('sc) }
implicit inline def t(inline sc: StringContext): T = ${ tMacro('sc) }
implicit inline def dt(inline sc: StringContext): DT = ${ dtMacro('sc) }
def withLocale[T](locale: String)(body: => T): T = TranslationRepository.withLocaleOverride(locale)(body)

private object TranslationRepository:
  private var cachedTranslations: Option[Map[String, Map[String, String]]] = None
  private var cachedFileSnapshot: Option[String] = None
  private val defaultLanguage = Language(LanguageTag.EN)
  private val localePreference: Parameter[Language] = Parameter()

  def translationTemplate(scExpr: Expr[StringContext])(using Quotes): String =
    val sc = scExpr match
      case '{ StringContext(${Varargs(parts)}*) } =>
        val evaluated = parts.map(_.valueOrAbort)
        new StringContext(evaluated*)
      case _ =>
        quotes.reflect.report.error("t interpolator only supports literal string contexts")
        new StringContext("")
    Template.stringContextToString(sc)

  def render(template: String, args: Vector[Any], translations: Map[String, Map[String, String]]): String =
    Template.applyTemplate(resolveTemplate(template, translations), args)

  def renderDoc(template: String, args: Vector[ToDoc], translations: Map[String, Map[String, String]])(using DocConf): Doc =
    val resolved = resolveTemplate(template, translations)
    val parts = Template.docPartsFromTemplate(resolved)
    Template.renderDocFromTemplateParts(parts, args)

  private def resolveTemplate(template: String, translations: Map[String, Map[String, String]]): String =
    val lang = detectLanguage()
    val locales = Seq(
      s"${lang.tag.name}_${lang.region.name}",
      lang.tag.name
    ).map(normalizeLocaleKey).distinct
    locales
      .iterator
      .flatMap(locale => translations.get(locale).flatMap(_.get(template)))
      .nextOption()
      .getOrElse(template)

  private def detectLanguage(): Language =
    localePreference.getOption.orElse {
      val env = sys.env
      val rawLocale = env
        .get("CHESTER_I18N_LOCALE")
        .orElse(env.get("LC_ALL"))
        .orElse(env.get("LANG"))
        .orElse(env.get("LANGUAGE"))
      rawLocale.flatMap(parseLanguage)
    }.getOrElse(defaultLanguage)

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

  def withLocaleOverride[T](locale: String)(body: => T): T =
    val language = parseLanguage(locale).getOrElse(defaultLanguage)
    localePreference.withValue(language)(body)

  def embeddedTranslations()(using Quotes): Expr[Map[String, Map[String, String]]] =
    val translations = loadTranslations()
    liftTranslations(translations)

  private def liftTranslations(map: Map[String, Map[String, String]])(using Quotes): Expr[Map[String, Map[String, String]]] =
    val localeEntries: List[Expr[(String, Map[String, String])]] = map.toList.map { case (k, v) =>
      val inner: List[Expr[(String, String)]] = v.toList.map { case (ik, iv) => '{ ${Expr(ik)} -> ${Expr(iv)} } }
      '{ ${Expr(k)} -> Map[String, String](${Expr.ofList(inner)}*) }
    }
    '{ Map[String, Map[String, String]](${Expr.ofList(localeEntries)}*) }

private def dMacro(scExpr: Expr[StringContext])(using Quotes): Expr[D] =
  val parts = scExpr match
    case '{ StringContext(${Varargs(ps)}*) } => ps.map(_.valueOrAbort).toVector
    case _ =>
      quotes.reflect.report.error("d interpolator requires literal StringContext")
      Vector.empty
  val partsExpr = Expr.ofSeq(parts.map(Expr(_)))
  '{
    new D {
      def d(args: ToDoc*)(using DocConf): Doc =
        Template.renderDocFromStringContext($partsExpr.toVector, args.toVector)
    }
  }

private def tMacro(sc: Expr[StringContext])(using Quotes): Expr[T] =
  val template = TranslationRepository.translationTemplate(sc)
  TranslationRepository.ensureTemplateRegistered(template)
  val templateExpr = Expr(template)(using summon[ToExpr[String]])
  val translationsExpr = TranslationRepository.embeddedTranslations()
  '{
    new T {
      def t(args: Any*): String =
        TranslationRepository.render($templateExpr, args.toVector, $translationsExpr)
    }
  }

private def dtMacro(sc: Expr[StringContext])(using Quotes): Expr[DT] =
  val template = TranslationRepository.translationTemplate(sc)
  TranslationRepository.ensureTemplateRegistered(template)
  val templateExpr = Expr(template)(using summon[ToExpr[String]])
  val translationsExpr = TranslationRepository.embeddedTranslations()
  '{
    new DT {
      def dt(args: ToDoc*)(using DocConf): Doc =
        TranslationRepository.renderDoc($templateExpr, args.toVector, $translationsExpr)
    }
  }
