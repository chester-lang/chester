package chester.parser

import chester.parser._
import chester.error.*
import chester.syntax.IdentifierRules.*
import chester.syntax.QualifiedIDString
import chester.syntax.concrete.*
import chester.utils.parse.*
import chester.utils._
import fastparse.*
import fastparse.NoWhitespace.*

import java.nio.file.{Files, Paths}
import scala.collection.immutable
import scala.util.*
import scala.scalajs.js.annotation._

implicit object FilePathImplJVM extends FilePathImpl {
  def load: Unit = {}

  require(impl == null, "FilePathImplJVM is already loaded")
  impl = this

  override def readContent(fileName: String): Either[ParseError, String] = {
    Try(readFileFrom(fileName)) match {
      case Success(content) =>
        Right(content)
      case Failure(exception) =>
        Left(ParseError(s"Failed to read file: ${exception.getMessage}", Pos.Zero))
    }
  }

  override def absolute(fileName: String): String = Paths.get(fileName).toAbsolutePath.toString
}
