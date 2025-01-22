package chester.error

trait Reporter[-T] {
  def apply(value: T): Unit
}

object StdErrReporter extends Reporter[Problem] {
  def apply(value: Problem): Unit = {
    println(value)
  }
}

extension [T](reporter: Reporter[T]) {
  def report(xs: Seq[T]): Unit = xs.foreach(reporter.apply)
}

class VectorReporter[T] extends Reporter[T] {
  private val buffer = scala.collection.mutable.ArrayBuffer[T]()

  def apply(value: T): Unit = buffer += value

  def getReports: Vector[T] = buffer.toVector
}