package chester.i18n

import chester.utils.doc.{DocConf, DocPrinter, StringPrinter}
import munit.FunSuite

class TranslationMacroTest extends FunSuite:
  given DocConf = DocConf.Default
  given DocPrinter = StringPrinter

  test("t interpolator uses zh_TW translation when forced") {
    val translated = withLocale("zh_TW") {
      t"Warning"
    }
    assertEquals(translated, "警告")
  }

  test("dt interpolator uses zh_TW translation when forced") {
    val doc = withLocale("zh_TW") {
      dt"Location"
    }
    val rendered = doc.toString
    assertEquals(rendered, "位置")
  }
