/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama.util

import kiama.util.Severities.{Error, Hint, Information, Severity, Warning}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Location, Position as LSPPosition, Range as LSPRange}

import java.nio.file.Paths

object Convert {
  def messageToDiagnostic[M <: Message](lspMessaging: Messaging[M])(message: M): Diagnostic = {
    diagnostic(message.range, lspMessaging.formatContent(message), message.severity)
  }

  def diagnostic(range: Option[kiama.util.Range], message: String, severity: Severity): Diagnostic = {
    val lspRange = range.map(convertRange).getOrElse(emptyRange)
    val lspSeverity = convertSeverity(severity)
    val lspDiagnostic = new Diagnostic(lspRange, message, lspSeverity, "effekt")
    lspDiagnostic
  }

  def emptyPosition = new LSPPosition(0, 0)
  def emptyRange = new LSPRange(emptyPosition, emptyPosition)
  def emptyLocation = new Location("<no-source>", emptyRange)

  def convertPosition(optPos: Option[Position]): LSPPosition =
    optPos.map(convertPosition).getOrElse(emptyPosition)

  def convertPosition(pos: Position): LSPPosition =
    new LSPPosition(pos.line - 1, pos.column - 1)

  def convertRange(optStart: Option[Position], optFinish: Option[Position]): LSPRange =
    new LSPRange(convertPosition(optStart), convertPosition(optFinish))

  def convertRange(r: kiama.util.Range): LSPRange =
    new LSPRange(convertPosition(r.from), convertPosition(r.to))

  def rangeToLocation(r: kiama.util.Range): Location =
    new Location(toURI(r.from.source.name), convertRange(r))

  def fromLSPPosition(position: LSPPosition, source: Source): Position =
    Position(position.getLine + 1, position.getCharacter + 1, source)

  def fromLSPRange(range: LSPRange, source: Source): kiama.util.Range =
    kiama.util.Range(
      fromLSPPosition(range.getStart, source),
      fromLSPPosition(range.getEnd, source)
    )

  def convertSeverity(severity: Severity): DiagnosticSeverity =
    severity match {
      case Error       => DiagnosticSeverity.Error
      case Warning     => DiagnosticSeverity.Warning
      case Information => DiagnosticSeverity.Information
      case Hint        => DiagnosticSeverity.Hint
    }

  // Convert a filename to a URI
  def toURI(filename: String): String = {
    if (filename.startsWith("file:") || filename.startsWith("vscode-notebook-cell:")) {
      // Already a URI or special scheme
      filename
    } else if (filename.startsWith("./") || filename.startsWith(".\\")) {
      // Remove the "./" or ".\\" prefix
      val relativePath = filename.substring(2)
      val cwd = System.getProperty("user.dir")
      val fullPath = Paths.get(cwd).resolve(relativePath).normalize()
      fullPath.toUri.toString
    } else {
      Paths.get(filename).toUri.toString
    }
  }
}
