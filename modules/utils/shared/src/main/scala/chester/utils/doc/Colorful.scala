package chester.utils.doc

import spire.math.Trilean

case class ColorfulPiece(text: String, style: Style)

case class Colorful(pieces: Vector[ColorfulPiece]) extends AnyVal {
  def ++(other: Colorful): Colorful = Colorful(pieces ++ other.pieces)

  def +:(piece: ColorfulPiece): Colorful = Colorful(piece +: pieces)

  def :+(piece: ColorfulPiece): Colorful = Colorful(pieces :+ piece)
}

object Colorful {
  val Empty: Colorful = Colorful(Vector.empty)
}

object ColorfulToHtml {
  def colorfulToHtml(colorful: Colorful): String = {
    colorful.pieces.map { piece =>
      val style = styleToCss(piece.style)
      s"<span style='$style'>${escapeHtml(piece.text)}</span>"
    }.mkString
  }

  private def styleToCss(style: Style): String = {
    val fgColor = style.foreground
      .map(fg => s"color: ${foregroundToCss(fg)};")
      .getOrElse("")
    val bgColor = style.background
      .map(bg => s"background-color: ${backgroundToCss(bg)};")
      .getOrElse("")
    val textStyles = stylingToCss(style.styling)
    s"$fgColor $bgColor $textStyles"
  }

  private def foregroundToCss(fg: Foreground): String = fg match {
    case Foreground.Black        => "black"
    case Foreground.Red          => "red"
    case Foreground.Green        => "green"
    case Foreground.Yellow       => "yellow"
    case Foreground.Blue         => "blue"
    case Foreground.Magenta      => "magenta"
    case Foreground.Cyan         => "cyan"
    case Foreground.LightGray    => "lightgray"
    case Foreground.DarkGray     => "darkgray"
    case Foreground.LightRed     => "lightcoral"
    case Foreground.LightGreen   => "lightgreen"
    case Foreground.LightYellow  => "lightyellow"
    case Foreground.LightBlue    => "lightblue"
    case Foreground.LightMagenta => "violet"
    case Foreground.LightCyan    => "lightcyan"
    case Foreground.White        => "white"
  }

  private def backgroundToCss(bg: Background): String = bg match {
    case Background.Black        => "black"
    case Background.Red          => "red"
    case Background.Green        => "green"
    case Background.Yellow       => "yellow"
    case Background.Blue         => "blue"
    case Background.Magenta      => "magenta"
    case Background.Cyan         => "cyan"
    case Background.LightGray    => "lightgray"
    case Background.DarkGray     => "darkgray"
    case Background.LightRed     => "lightcoral"
    case Background.LightGreen   => "lightgreen"
    case Background.LightYellow  => "lightyellow"
    case Background.LightBlue    => "lightblue"
    case Background.LightMagenta => "violet"
    case Background.LightCyan    => "lightcyan"
    case Background.White        => "white"
  }

  private def stylingToCss(stylings: Stylings): String = {
    val boldStyle = stylings.bold match {
      case Trilean.True    => "font-weight: bold;"
      case Trilean.False   => "font-weight: normal;"
      case Trilean.Unknown => ""
    }
    // Add more styles (italic, underline, etc.) if your Stylings class supports them
    boldStyle
  }

  private def escapeHtml(text: String): String = {
    text
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&#39;")
  }
}
