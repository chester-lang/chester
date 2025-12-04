/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

/**
 * Record of a source position at a particular line and column relative to
 * a given source. The line and column numbers are one-indexed.
 */
case class Position(line: Int, column: Int, source: Source) {

  /**
   * Format this position. The result is of the form `/foo/bar.txt:2:10:` if
   * a source is associated with the position and the source has a name, or
   * of the form `2:10:` otherwise. In each case the numbers are line followed
   * by column.
   */
  lazy val format: String = {
    val name = if (source.name == "") "" else s"${source.name}:"
    s"$name$line:$column:"
  }

  /**
   * Turn this position into a string that summarises the context of the input
   * referred to by the position. If the position has a source that provides
   * access to its lines then the context is the line containing the position
   * followed by a line containing a caret pointer. Otherwise, return `None`.
   */
  lazy val optContext: Option[String] =
    source.optLineContents(line).map(s => s"$s\n${" " * (column - 1)}^")

  /**
   * Return the offset that this position refers to in its source. `None`
   * is returned if the position is not valid for its source.
   */
  lazy val optOffset: Option[Int] =
    source.positionToOffset(this)

  /**
   * Does this position occur no later than `p`? The two positions
   * are assumed to refer to the same source.
   * If the second position falls outside the source, but the first
   * one is inside, the first one is considered to be before the second.
   * False is returned if both positions are invalid.
   */
  def <=(p: Position): Boolean = {
    (optOffset, p.optOffset) match {
      case (Some(l), Some(r)) =>
        l <= r
      case (Some(_), None) =>
        true
      case (l, r) =>
        false
    }
  }

  /**
   * Does this position occur before `p`? The two positions are assumed
   * to refer to the same source.
   * If the second position falls outside the source, but the first
   * one is inside, the first one is considered to be before the second.
   * False is returned if both positions are invalid.
   */
  def <(p: Position): Boolean = {
    (optOffset, p.optOffset) match {
      case (Some(l), Some(r)) =>
        l < r
      case (Some(_), None) =>
        true
      case (l, r) =>
        false
    }
  }

  /**
   * Does this position occur between two other positions, in the same
   * source, inclusive of start position and exclusive of finish position?
   */
  def between(start: Position, finish: Position): Boolean =
    (start <= this) && (this < finish)
}

case class Range(from: Position, to: Position)

object Range {
  def empty(source: Source): Range = Range(Position(1, 1, source), Position(1, 1, source))
}
