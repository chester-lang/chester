package chester.core

import upickle.default.*

/** Distinguishes line vs. block comments for CST rendering. */
enum CommentKind derives ReadWriter:
  case Line
  case Block
