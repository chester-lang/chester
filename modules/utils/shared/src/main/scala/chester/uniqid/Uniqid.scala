package chester.uniqid

import upickle.default.*
import spire.math.Natural
import chester.utils.{Nat, asInt, given}
import scala.language.experimental.genericNumberLiterals

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.language.strictEquality

private val uniqIdCounter = new AtomicInteger(0)

type Uniqid = UniqidOf[Any]

private val rwUniqID: ReadWriter[UniqidOf[Any]] = readwriter[Int].bimap(
  _.id.asInt,
  x => UniqidOf(Nat(x))
)

implicit inline def rwUniqIDOf[T]: ReadWriter[UniqidOf[T]] = rwUniqID.asInstanceOf[ReadWriter[UniqidOf[T]]]

case class UniqidOf[+A] private[uniqid] (id: spire.math.Natural) derives CanEqual {}

type UniqidOffset = spire.math.Natural

case class UniqIdRange(start: UniqidOffset, end: UniqidOffset) derives ReadWriter {
  require(start <= end, s"Invalid range: $start > $end")

  def size: spire.math.Natural = end - start
}

extension (x: Uniqid) {
  def asof[T]: UniqidOf[T] = x.asInstanceOf[UniqidOf[T]]
}

extension [T](x: UniqidOf[T]) {
  def asid: Uniqid = x
  def rerange(current: UniqIdRange, target: UniqIdRange): UniqidOf[T] = {
    require(
      current.start <= x.id && x.id < current.end,
      s"Invalid range: $current, $x"
    )
    val offset = x.id - current.start
    UniqidOf(target.start + offset)
  }
}

trait UniqidCollector {
  def apply[T](x: UniqidOf[T]): Unit = ()
}

trait SupportsUniqidCollection extends Any {
  def collectUniqids(collector: UniqidCollector): Unit
}

trait UniqidReplacer {
  def apply[T](x: UniqidOf[T]): UniqidOf[T] = x
}

trait SupportsUniqidMapping extends Any {
  def mapUniqids(mapper: UniqidReplacer): Any
}

trait ContainsUniqid extends Any with SupportsUniqidCollection with SupportsUniqidMapping {
  lazy val uniqIdRange: UniqIdRange = Uniqid.calculateRange(this)
}

trait OnlyHasUniqid extends Any {
  def uniqId: Uniqid
}

trait HasUniqid extends Any with ContainsUniqid with OnlyHasUniqid {}

object Uniqid {
  def make[T]: UniqidOf[T] = UniqidOf(Nat(uniqIdCounter.getAndIncrement()))

  def requireRange(size: spire.math.Natural): UniqIdRange = {
    val sizeInt = size.asInt
    val start = uniqIdCounter.getAndAdd(sizeInt)
    UniqIdRange(Nat(start), Nat(start + sizeInt))
  }

  def currentOffset(): UniqidOffset = Nat(uniqIdCounter.get())

  def captureRange[T](f: => T): (UniqIdRange, T) = {
    val start = currentOffset()
    val result = f
    val end = currentOffset()
    (UniqIdRange(start, end), result)
  }

  def is(x: Any): Boolean = x match {
    case UniqidOf(_) => true
    case _           => false
  }

  def calculateRange[T <: ContainsUniqid](x: T): UniqIdRange = {
    val currentRangeCollect = new mutable.ArrayDeque[Uniqid]()
    val collector: UniqidCollector = new UniqidCollector {
      override def apply[T](id: UniqidOf[T]): Unit =
        currentRangeCollect.append(id)
    }
    x.collectUniqids(collector)
    import spire.compat.ordering
    if (currentRangeCollect.isEmpty) UniqIdRange(currentOffset(), currentOffset())
    else UniqIdRange(currentRangeCollect.map(_.id).min, currentRangeCollect.map(_.id).max + (1: Natural))
  }

  case class GiveNewRangeResult[T <: ContainsUniqid](
      oldRange: UniqIdRange,
      newRange: UniqIdRange,
      result: T
  )

  def giveNewRange[T <: ContainsUniqid](
      x: T
  ): GiveNewRangeResult[T] = {
    val currentRange = x.uniqIdRange
    val newRange = requireRange(currentRange.size)
    val mapper: UniqidReplacer = new UniqidReplacer {
      override def apply[U](id: UniqidOf[U]): UniqidOf[U] =
        id.rerange(currentRange, newRange)
    }
    GiveNewRangeResult(currentRange, newRange, x.mapUniqids(mapper).asInstanceOf[T])
  }
}
