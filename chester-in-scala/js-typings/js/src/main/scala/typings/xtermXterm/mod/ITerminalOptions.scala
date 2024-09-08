package typings.xtermXterm.mod

import typings.xtermXterm.xtermXtermStrings.alt
import typings.xtermXterm.xtermXtermStrings.bar
import typings.xtermXterm.xtermXtermStrings.block
import typings.xtermXterm.xtermXtermStrings.ctrl
import typings.xtermXterm.xtermXtermStrings.none
import typings.xtermXterm.xtermXtermStrings.outline
import typings.xtermXterm.xtermXtermStrings.shift
import typings.xtermXterm.xtermXtermStrings.underline
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait ITerminalOptions extends StObject {
  
  /**
    * Whether to allow the use of proposed API. When false, any usage of APIs
    * marked as experimental/proposed will throw an error. The default is
    * false.
    */
  var allowProposedApi: js.UndefOr[Boolean] = js.undefined
  
  /**
    * Whether background should support non-opaque color. It must be set before
    * executing the `Terminal.open()` method and can't be changed later without
    * executing it again. Note that enabling this can negatively impact
    * performance.
    */
  var allowTransparency: js.UndefOr[Boolean] = js.undefined
  
  /**
    * If enabled, alt + click will move the prompt cursor to position
    * underneath the mouse. The default is true.
    */
  var altClickMovesCursor: js.UndefOr[Boolean] = js.undefined
  
  /**
    * When enabled the cursor will be set to the beginning of the next line
    * with every new line. This is equivalent to sending '\r\n' for each '\n'.
    * Normally the termios settings of the underlying PTY deals with the
    * translation of '\n' to '\r\n' and this setting should not be used. If you
    * deal with data from a non-PTY related source, this settings might be
    * useful.
    */
  var convertEol: js.UndefOr[Boolean] = js.undefined
  
  /**
    * Whether the cursor blinks.
    */
  var cursorBlink: js.UndefOr[Boolean] = js.undefined
  
  /**
    * The style of the cursor when the terminal is not focused.
    */
  var cursorInactiveStyle: js.UndefOr[outline | block | bar | underline | none] = js.undefined
  
  /**
    * The style of the cursor when the terminal is focused.
    */
  var cursorStyle: js.UndefOr[block | underline | bar] = js.undefined
  
  /**
    * The width of the cursor in CSS pixels when `cursorStyle` is set to 'bar'.
    */
  var cursorWidth: js.UndefOr[Double] = js.undefined
  
  /**
    * Whether to draw custom glyphs for block element and box drawing
    * characters instead of using the font. This should typically result in
    * better rendering with continuous lines, even when line height and letter
    * spacing is used. Note that this doesn't work with the DOM renderer which
    * renders all characters using the font. The default is true.
    */
  var customGlyphs: js.UndefOr[Boolean] = js.undefined
  
  /**
    * Whether input should be disabled.
    */
  var disableStdin: js.UndefOr[Boolean] = js.undefined
  
  /**
    * A {@link Document} to use instead of the one that xterm.js was attached
    * to. The purpose of this is to improve support in multi-window
    * applications where HTML elements may be references across multiple
    * windows which can cause problems with `instanceof`.
    *
    * The type is `any` because using `Document` can cause TS to have
    * performance/compiler problems.
    */
  var documentOverride: js.UndefOr[Any | Null] = js.undefined
  
  /**
    * Whether to draw bold text in bright colors. The default is true.
    */
  var drawBoldTextInBrightColors: js.UndefOr[Boolean] = js.undefined
  
  /**
    * The modifier key hold to multiply scroll speed.
    */
  var fastScrollModifier: js.UndefOr[none | alt | ctrl | shift] = js.undefined
  
  /**
    * The scroll speed multiplier used for fast scrolling.
    */
  var fastScrollSensitivity: js.UndefOr[Double] = js.undefined
  
  /**
    * The font family used to render text.
    */
  var fontFamily: js.UndefOr[String] = js.undefined
  
  /**
    * The font size used to render text.
    */
  var fontSize: js.UndefOr[Double] = js.undefined
  
  /**
    * The font weight used to render non-bold text.
    */
  var fontWeight: js.UndefOr[FontWeight] = js.undefined
  
  /**
    * The font weight used to render bold text.
    */
  var fontWeightBold: js.UndefOr[FontWeight] = js.undefined
  
  /**
    * Whether to ignore the bracketed paste mode. When true, this will always
    * paste without the `\x1b[200~` and `\x1b[201~` sequences, even when the
    * shell enables bracketed mode.
    */
  var ignoreBracketedPasteMode: js.UndefOr[Boolean] = js.undefined
  
  /**
    * The spacing in whole pixels between characters.
    */
  var letterSpacing: js.UndefOr[Double] = js.undefined
  
  /**
    * The line height used to render text.
    */
  var lineHeight: js.UndefOr[Double] = js.undefined
  
  /**
    * The handler for OSC 8 hyperlinks. Links will use the `confirm` browser
    * API with a strongly worded warning if no link handler is set.
    *
    * When setting this, consider the security of users opening these links,
    * at a minimum there should be a tooltip or a prompt when hovering or
    * activating the link respectively. An example of what might be possible is
    * a terminal app writing link in the form `javascript:...` that runs some
    * javascript, a safe approach to prevent that is to validate the link
    * starts with http(s)://.
    */
  var linkHandler: js.UndefOr[ILinkHandler | Null] = js.undefined
  
  /**
    * What log level to use, this will log for all levels below and including
    * what is set:
    *
    * 1. trace
    * 2. debug
    * 3. info (default)
    * 4. warn
    * 5. error
    * 6. off
    */
  var logLevel: js.UndefOr[LogLevel] = js.undefined
  
  /**
    * A logger to use instead of `console`.
    */
  var logger: js.UndefOr[ILogger | Null] = js.undefined
  
  /**
    * Whether holding a modifier key will force normal selection behavior,
    * regardless of whether the terminal is in mouse events mode. This will
    * also prevent mouse events from being emitted by the terminal. For
    * example, this allows you to use xterm.js' regular selection inside tmux
    * with mouse mode enabled.
    */
  var macOptionClickForcesSelection: js.UndefOr[Boolean] = js.undefined
  
  /**
    * Whether to treat option as the meta key.
    */
  var macOptionIsMeta: js.UndefOr[Boolean] = js.undefined
  
  /**
    * The minimum contrast ratio for text in the terminal, setting this will
    * change the foreground color dynamically depending on whether the contrast
    * ratio is met. Example values:
    *
    * - 1: The default, do nothing.
    * - 4.5: Minimum for WCAG AA compliance.
    * - 7: Minimum for WCAG AAA compliance.
    * - 21: White on black or black on white.
    */
  var minimumContrastRatio: js.UndefOr[Double] = js.undefined
  
  /**
    * The width, in pixels, of the canvas for the overview ruler. The overview
    * ruler will be hidden when not set.
    */
  var overviewRulerWidth: js.UndefOr[Double] = js.undefined
  
  /**
    * Whether to rescale glyphs horizontally that are a single cell wide but
    * have glyphs that would overlap following cell(s). This typically happens
    * for ambiguous width characters (eg. the roman numeral characters U+2160+)
    * which aren't featured in monospace fonts. This is an important feature
    * for achieving GB18030 compliance.
    *
    * The following glyphs will never be rescaled:
    *
    * - Emoji glyphs
    * - Powerline glyphs
    * - Nerd font glyphs
    *
    * Note that this doesn't work with the DOM renderer. The default is false.
    */
  var rescaleOverlappingGlyphs: js.UndefOr[Boolean] = js.undefined
  
  /**
    * Whether to select the word under the cursor on right click, this is
    * standard behavior in a lot of macOS applications.
    */
  var rightClickSelectsWord: js.UndefOr[Boolean] = js.undefined
  
  /**
    * Whether screen reader support is enabled. When on this will expose
    * supporting elements in the DOM to support NVDA on Windows and VoiceOver
    * on macOS.
    */
  var screenReaderMode: js.UndefOr[Boolean] = js.undefined
  
  /**
    * Whether to scroll to the bottom whenever there is some user input. The
    * default is true.
    */
  var scrollOnUserInput: js.UndefOr[Boolean] = js.undefined
  
  /**
    * The scrolling speed multiplier used for adjusting normal scrolling speed.
    */
  var scrollSensitivity: js.UndefOr[Double] = js.undefined
  
  /**
    * The amount of scrollback in the terminal. Scrollback is the amount of
    * rows that are retained when lines are scrolled beyond the initial
    * viewport. Defaults to 1000.
    */
  var scrollback: js.UndefOr[Double] = js.undefined
  
  /**
    * The duration to smoothly scroll between the origin and the target in
    * milliseconds. Set to 0 to disable smooth scrolling and scroll instantly.
    */
  var smoothScrollDuration: js.UndefOr[Double] = js.undefined
  
  /**
    * The size of tab stops in the terminal.
    */
  var tabStopWidth: js.UndefOr[Double] = js.undefined
  
  /**
    * The color theme of the terminal.
    */
  var theme: js.UndefOr[ITheme] = js.undefined
  
  /**
    * Enable various window manipulation and report features.
    * All features are disabled by default for security reasons.
    */
  var windowOptions: js.UndefOr[IWindowOptions] = js.undefined
  
  /**
    * Whether "Windows mode" is enabled. Because Windows backends winpty and
    * conpty operate by doing line wrapping on their side, xterm.js does not
    * have access to wrapped lines. When Windows mode is enabled the following
    * changes will be in effect:
    *
    * - Reflow is disabled.
    * - Lines are assumed to be wrapped if the last character of the line is
    *   not whitespace.
    *
    * When using conpty on Windows 11 version >= 21376, it is recommended to
    * disable this because native text wrapping sequences are output correctly
    * thanks to https://github.com/microsoft/terminal/issues/405
    *
    * @deprecated Use {@link windowsPty}. This value will be ignored if
    * windowsPty is set.
    */
  var windowsMode: js.UndefOr[Boolean] = js.undefined
  
  /**
    * Compatibility information when the pty is known to be hosted on Windows.
    * Setting this will turn on certain heuristics/workarounds depending on the
    * values:
    *
    * - `if (backend !== undefined || buildNumber !== undefined)`
    *   - When increasing the rows in the terminal, the amount increased into
    *     the scrollback. This is done because ConPTY does not behave like
    *     expect scrollback to come back into the viewport, instead it makes
    *     empty rows at of the viewport. Not having this behavior can result in
    *     missing data as the rows get replaced.
    * - `if !(backend === 'conpty' && buildNumber >= 21376)`
    *   - Reflow is disabled
    *   - Lines are assumed to be wrapped if the last character of the line is
    *     not whitespace.
    */
  var windowsPty: js.UndefOr[IWindowsPty] = js.undefined
  
  /**
    * A string containing all characters that are considered word separated by
    * the double click to select work logic.
    */
  var wordSeparator: js.UndefOr[String] = js.undefined
}
object ITerminalOptions {
  
  inline def apply(): ITerminalOptions = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[ITerminalOptions]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: ITerminalOptions] (val x: Self) extends AnyVal {
    
    inline def setAllowProposedApi(value: Boolean): Self = StObject.set(x, "allowProposedApi", value.asInstanceOf[js.Any])
    
    inline def setAllowProposedApiUndefined: Self = StObject.set(x, "allowProposedApi", js.undefined)
    
    inline def setAllowTransparency(value: Boolean): Self = StObject.set(x, "allowTransparency", value.asInstanceOf[js.Any])
    
    inline def setAllowTransparencyUndefined: Self = StObject.set(x, "allowTransparency", js.undefined)
    
    inline def setAltClickMovesCursor(value: Boolean): Self = StObject.set(x, "altClickMovesCursor", value.asInstanceOf[js.Any])
    
    inline def setAltClickMovesCursorUndefined: Self = StObject.set(x, "altClickMovesCursor", js.undefined)
    
    inline def setConvertEol(value: Boolean): Self = StObject.set(x, "convertEol", value.asInstanceOf[js.Any])
    
    inline def setConvertEolUndefined: Self = StObject.set(x, "convertEol", js.undefined)
    
    inline def setCursorBlink(value: Boolean): Self = StObject.set(x, "cursorBlink", value.asInstanceOf[js.Any])
    
    inline def setCursorBlinkUndefined: Self = StObject.set(x, "cursorBlink", js.undefined)
    
    inline def setCursorInactiveStyle(value: outline | block | bar | underline | none): Self = StObject.set(x, "cursorInactiveStyle", value.asInstanceOf[js.Any])
    
    inline def setCursorInactiveStyleUndefined: Self = StObject.set(x, "cursorInactiveStyle", js.undefined)
    
    inline def setCursorStyle(value: block | underline | bar): Self = StObject.set(x, "cursorStyle", value.asInstanceOf[js.Any])
    
    inline def setCursorStyleUndefined: Self = StObject.set(x, "cursorStyle", js.undefined)
    
    inline def setCursorWidth(value: Double): Self = StObject.set(x, "cursorWidth", value.asInstanceOf[js.Any])
    
    inline def setCursorWidthUndefined: Self = StObject.set(x, "cursorWidth", js.undefined)
    
    inline def setCustomGlyphs(value: Boolean): Self = StObject.set(x, "customGlyphs", value.asInstanceOf[js.Any])
    
    inline def setCustomGlyphsUndefined: Self = StObject.set(x, "customGlyphs", js.undefined)
    
    inline def setDisableStdin(value: Boolean): Self = StObject.set(x, "disableStdin", value.asInstanceOf[js.Any])
    
    inline def setDisableStdinUndefined: Self = StObject.set(x, "disableStdin", js.undefined)
    
    inline def setDocumentOverride(value: Any): Self = StObject.set(x, "documentOverride", value.asInstanceOf[js.Any])
    
    inline def setDocumentOverrideNull: Self = StObject.set(x, "documentOverride", null)
    
    inline def setDocumentOverrideUndefined: Self = StObject.set(x, "documentOverride", js.undefined)
    
    inline def setDrawBoldTextInBrightColors(value: Boolean): Self = StObject.set(x, "drawBoldTextInBrightColors", value.asInstanceOf[js.Any])
    
    inline def setDrawBoldTextInBrightColorsUndefined: Self = StObject.set(x, "drawBoldTextInBrightColors", js.undefined)
    
    inline def setFastScrollModifier(value: none | alt | ctrl | shift): Self = StObject.set(x, "fastScrollModifier", value.asInstanceOf[js.Any])
    
    inline def setFastScrollModifierUndefined: Self = StObject.set(x, "fastScrollModifier", js.undefined)
    
    inline def setFastScrollSensitivity(value: Double): Self = StObject.set(x, "fastScrollSensitivity", value.asInstanceOf[js.Any])
    
    inline def setFastScrollSensitivityUndefined: Self = StObject.set(x, "fastScrollSensitivity", js.undefined)
    
    inline def setFontFamily(value: String): Self = StObject.set(x, "fontFamily", value.asInstanceOf[js.Any])
    
    inline def setFontFamilyUndefined: Self = StObject.set(x, "fontFamily", js.undefined)
    
    inline def setFontSize(value: Double): Self = StObject.set(x, "fontSize", value.asInstanceOf[js.Any])
    
    inline def setFontSizeUndefined: Self = StObject.set(x, "fontSize", js.undefined)
    
    inline def setFontWeight(value: FontWeight): Self = StObject.set(x, "fontWeight", value.asInstanceOf[js.Any])
    
    inline def setFontWeightBold(value: FontWeight): Self = StObject.set(x, "fontWeightBold", value.asInstanceOf[js.Any])
    
    inline def setFontWeightBoldUndefined: Self = StObject.set(x, "fontWeightBold", js.undefined)
    
    inline def setFontWeightUndefined: Self = StObject.set(x, "fontWeight", js.undefined)
    
    inline def setIgnoreBracketedPasteMode(value: Boolean): Self = StObject.set(x, "ignoreBracketedPasteMode", value.asInstanceOf[js.Any])
    
    inline def setIgnoreBracketedPasteModeUndefined: Self = StObject.set(x, "ignoreBracketedPasteMode", js.undefined)
    
    inline def setLetterSpacing(value: Double): Self = StObject.set(x, "letterSpacing", value.asInstanceOf[js.Any])
    
    inline def setLetterSpacingUndefined: Self = StObject.set(x, "letterSpacing", js.undefined)
    
    inline def setLineHeight(value: Double): Self = StObject.set(x, "lineHeight", value.asInstanceOf[js.Any])
    
    inline def setLineHeightUndefined: Self = StObject.set(x, "lineHeight", js.undefined)
    
    inline def setLinkHandler(value: ILinkHandler): Self = StObject.set(x, "linkHandler", value.asInstanceOf[js.Any])
    
    inline def setLinkHandlerNull: Self = StObject.set(x, "linkHandler", null)
    
    inline def setLinkHandlerUndefined: Self = StObject.set(x, "linkHandler", js.undefined)
    
    inline def setLogLevel(value: LogLevel): Self = StObject.set(x, "logLevel", value.asInstanceOf[js.Any])
    
    inline def setLogLevelUndefined: Self = StObject.set(x, "logLevel", js.undefined)
    
    inline def setLogger(value: ILogger): Self = StObject.set(x, "logger", value.asInstanceOf[js.Any])
    
    inline def setLoggerNull: Self = StObject.set(x, "logger", null)
    
    inline def setLoggerUndefined: Self = StObject.set(x, "logger", js.undefined)
    
    inline def setMacOptionClickForcesSelection(value: Boolean): Self = StObject.set(x, "macOptionClickForcesSelection", value.asInstanceOf[js.Any])
    
    inline def setMacOptionClickForcesSelectionUndefined: Self = StObject.set(x, "macOptionClickForcesSelection", js.undefined)
    
    inline def setMacOptionIsMeta(value: Boolean): Self = StObject.set(x, "macOptionIsMeta", value.asInstanceOf[js.Any])
    
    inline def setMacOptionIsMetaUndefined: Self = StObject.set(x, "macOptionIsMeta", js.undefined)
    
    inline def setMinimumContrastRatio(value: Double): Self = StObject.set(x, "minimumContrastRatio", value.asInstanceOf[js.Any])
    
    inline def setMinimumContrastRatioUndefined: Self = StObject.set(x, "minimumContrastRatio", js.undefined)
    
    inline def setOverviewRulerWidth(value: Double): Self = StObject.set(x, "overviewRulerWidth", value.asInstanceOf[js.Any])
    
    inline def setOverviewRulerWidthUndefined: Self = StObject.set(x, "overviewRulerWidth", js.undefined)
    
    inline def setRescaleOverlappingGlyphs(value: Boolean): Self = StObject.set(x, "rescaleOverlappingGlyphs", value.asInstanceOf[js.Any])
    
    inline def setRescaleOverlappingGlyphsUndefined: Self = StObject.set(x, "rescaleOverlappingGlyphs", js.undefined)
    
    inline def setRightClickSelectsWord(value: Boolean): Self = StObject.set(x, "rightClickSelectsWord", value.asInstanceOf[js.Any])
    
    inline def setRightClickSelectsWordUndefined: Self = StObject.set(x, "rightClickSelectsWord", js.undefined)
    
    inline def setScreenReaderMode(value: Boolean): Self = StObject.set(x, "screenReaderMode", value.asInstanceOf[js.Any])
    
    inline def setScreenReaderModeUndefined: Self = StObject.set(x, "screenReaderMode", js.undefined)
    
    inline def setScrollOnUserInput(value: Boolean): Self = StObject.set(x, "scrollOnUserInput", value.asInstanceOf[js.Any])
    
    inline def setScrollOnUserInputUndefined: Self = StObject.set(x, "scrollOnUserInput", js.undefined)
    
    inline def setScrollSensitivity(value: Double): Self = StObject.set(x, "scrollSensitivity", value.asInstanceOf[js.Any])
    
    inline def setScrollSensitivityUndefined: Self = StObject.set(x, "scrollSensitivity", js.undefined)
    
    inline def setScrollback(value: Double): Self = StObject.set(x, "scrollback", value.asInstanceOf[js.Any])
    
    inline def setScrollbackUndefined: Self = StObject.set(x, "scrollback", js.undefined)
    
    inline def setSmoothScrollDuration(value: Double): Self = StObject.set(x, "smoothScrollDuration", value.asInstanceOf[js.Any])
    
    inline def setSmoothScrollDurationUndefined: Self = StObject.set(x, "smoothScrollDuration", js.undefined)
    
    inline def setTabStopWidth(value: Double): Self = StObject.set(x, "tabStopWidth", value.asInstanceOf[js.Any])
    
    inline def setTabStopWidthUndefined: Self = StObject.set(x, "tabStopWidth", js.undefined)
    
    inline def setTheme(value: ITheme): Self = StObject.set(x, "theme", value.asInstanceOf[js.Any])
    
    inline def setThemeUndefined: Self = StObject.set(x, "theme", js.undefined)
    
    inline def setWindowOptions(value: IWindowOptions): Self = StObject.set(x, "windowOptions", value.asInstanceOf[js.Any])
    
    inline def setWindowOptionsUndefined: Self = StObject.set(x, "windowOptions", js.undefined)
    
    inline def setWindowsMode(value: Boolean): Self = StObject.set(x, "windowsMode", value.asInstanceOf[js.Any])
    
    inline def setWindowsModeUndefined: Self = StObject.set(x, "windowsMode", js.undefined)
    
    inline def setWindowsPty(value: IWindowsPty): Self = StObject.set(x, "windowsPty", value.asInstanceOf[js.Any])
    
    inline def setWindowsPtyUndefined: Self = StObject.set(x, "windowsPty", js.undefined)
    
    inline def setWordSeparator(value: String): Self = StObject.set(x, "wordSeparator", value.asInstanceOf[js.Any])
    
    inline def setWordSeparatorUndefined: Self = StObject.set(x, "wordSeparator", js.undefined)
  }
}
