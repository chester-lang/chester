package chester.utils

import com.eed3si9n.ifdef.*

@ifndef("jdk17")
def codePointIsEmoji(codePoint: Int): Boolean = {
  (codePoint >= 0x1f600 && codePoint <= 0x1f64f) || // Emoticons
    (codePoint >= 0x1f300 && codePoint <= 0x1f5ff) || // Miscellaneous Symbols and Pictographs
    (codePoint >= 0x1f680 && codePoint <= 0x1f6ff) || // Transport and Map Symbols
    (codePoint >= 0x1f900 && codePoint <= 0x1f9ff) || // Supplemental Symbols and Pictographs
    (codePoint >= 0xe000 && codePoint <= 0xf8ff) || // Supplementary Private Use Area A
    (codePoint >= 0xf0000 && codePoint <= 0xfffff) || // Supplementary Private Use Area B
    (codePoint >= 0x100000 && codePoint <= 0x10ffff) // Supplementary Private Use Area B continuation
}

@ifdef("jdk17")
def codePointIsEmoji(codePoint: Int): Boolean = {
  val block = Character.UnicodeBlock.of(codePoint)

  block == Character.UnicodeBlock.EMOTICONS ||
  block == Character.UnicodeBlock.MISCELLANEOUS_SYMBOLS_AND_PICTOGRAPHS ||
  block == Character.UnicodeBlock.TRANSPORT_AND_MAP_SYMBOLS ||
  block == Character.UnicodeBlock.SUPPLEMENTAL_SYMBOLS_AND_PICTOGRAPHS ||
  block == Character.UnicodeBlock.SUPPLEMENTARY_PRIVATE_USE_AREA_A ||
  block == Character.UnicodeBlock.SUPPLEMENTARY_PRIVATE_USE_AREA_B
}
