package chester.utils.io
import scala.language.implicitConversions

trait PathOps[T] {
  def of(path: String): T

  def join(p1: T, p2: String): T

  def asString(p: T): String

  def baseName(p: T): String

  def isAbsolute(p: T): Boolean

  def isRelative(p: T): Boolean
}

extension [T](p: T)(using ops: PathOps[T]) {
  inline def /(inline p2: String): T = ops.join(p, p2)
}

implicit inline def stringToPath[T](inline path: String)(using
    inline ops: PathOps[T]
): T = ops.of(path)

implicit object PathOpsString extends PathOps[String] {
  inline def of(path: String): String = path

  inline def join(p1: String, p2: String): String = p1 + "/" + p2

  inline def asString(p: String): String = p

  inline def baseName(p: String): String = p.split("/").last

  inline def isAbsolute(p: String): Boolean = p.startsWith("/") || p.matches("^[A-Za-z]:\\\\.*") || p.startsWith("\\\\")

  inline def isRelative(p: String): Boolean = !isAbsolute(p)
}
