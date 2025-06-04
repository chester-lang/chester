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

  private def getOption[T](key: DocConfKey[T]): Option[T] =
    options.get(key).map(_.asInstanceOf[T])

  private def getOrElse__[T](key: DocConfKey[T], default: T): T =
    getOption(key).getOrElse(default)

  def updated[T](key: DocConfKey[T], value: T): DocConf =
    DocConf(options.updated(key, value))
}

case class PrettierKeyValue[T](key: DocConfKey[T], value: T)

implicit def tuple2PrettyKeyValue[T](
    tuple: (DocConfKey[T], T)
): PrettierKeyValue[T] = PrettierKeyValue(tuple._1, tuple._2)
implicit def prettyKeyValue2Tuple[T](
    kv: PrettierKeyValue[T]
): (DocConfKey[T], T) = (kv.key, kv.value)

object DocConf {
  val Default: DocConf = DocConf(Map.empty)

  def apply(options: PrettierKeyValue[?]*): DocConf = DocConf(
    options.map(prettyKeyValue2Tuple).toMap
  )
}
