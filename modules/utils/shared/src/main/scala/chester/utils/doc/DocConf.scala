package chester.utils.doc
import scala.language.implicitConversions

trait DocConfKey[T] {
  def default: T

  def get(using options: DocConf): T = options.get(this)
}

case class DocConf(x: Map[DocConfKey[?], Any]) extends AnyVal {
  private inline def options: Map[DocConfKey[?], Any] = x
  def get[T](key: DocConfKey[T]): T = {
    val default: T = key.default
    getOrElse__[T](key, default)
  }

  private def getOption[T](key: DocConfKey[T]): Option[T] = {
    options.get(key).map(_.asInstanceOf[T])
  }

  private def getOrElse__[T](key: DocConfKey[T], default: T): T = {
    getOption(key).getOrElse(default)
  }

  def updated[T](key: DocConfKey[T], value: T): DocConf = {
    DocConf(options.updated(key, value))
  }
}

case class DocConfKeyVal[T](key: DocConfKey[T], value: T)

implicit def tuple2docConfKeyVal[T](
    tuple: (DocConfKey[T], T)
): DocConfKeyVal[T] = DocConfKeyVal(tuple._1, tuple._2)
implicit def docConfKeyVal2tuple[T](
    kv: DocConfKeyVal[T]
): (DocConfKey[T], T) = (kv.key, kv.value)

object DocConf {
  val Default: DocConf = DocConf(Map.empty)

  def apply(options: DocConfKeyVal[?]*): DocConf = DocConf(
    options.map(docConfKeyVal2tuple).toMap
  )
}
