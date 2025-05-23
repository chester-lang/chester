package chester.utils.propagator

import chester.uniqid.{Uniqid, UniqidOf}
import chester.i18n.*
import chester.utils.cell.*

import scala.collection.mutable

trait ProvideMutable extends ProvideImpl {
  class HoldCell[+T <: CellContent[?, ?]](val uniqId: UniqidOf[Impl[?]], value: T) {
    var store: CellContent[?, ?] = value
    var didChange: Boolean = false
    var readingPropagators: Vector[PIdOf[Propagator[?]]] = Vector.empty
    var zonkingPropagators: Vector[PIdOf[Propagator[?]]] = Vector.empty

    inline def noAnyValue: Boolean = store.noAnyValue
  }

  type CIdOf[+T <: CellContent[?, ?]] = HoldCell[T]

  class HoldPropagator[+T <: Propagator[?]](
      val uniqId: UniqidOf[Impl[?]],
      value: T
  ) {
    var store: Propagator[?] = value
    var alive: Boolean = true
  }

  type PIdOf[+T <: Propagator[?]] = HoldPropagator[T]

  override def isCId(x: Any): Boolean = {
    val result = x.isInstanceOf[HoldCell[?]]
    result
  }

  override def assumeCId(x: Any): CIdOf[CellContent[?, ?]] = {
    require(isCId(x))
    x.asInstanceOf[CIdOf[CellContent[?, ?]]]
  }

  override def stateAbilityImpl[Ability]: StateOps[Ability] =
    Impl[Ability]()

  class Impl[Ops](
      val uniqId: UniqidOf[Impl[Ops]] = Uniqid.generate[Impl[Ops]]
  ) extends StateOps[Ops] {
    var didChanged: mutable.ArrayDeque[CIdOf[?]] = mutable.ArrayDeque.empty

    override def readCell[T <: CellContent[?, ?]](id: CIdOf[T]): Option[T] = {
      require(id.uniqId == uniqId)
      Some(id.store.asInstanceOf[T])
    }

    override def update[T <: CellContent[?, ?]](id: CIdOf[T], f: T => T)(using
        Ops
    ): Unit = {
      didSomething = true
      require(id.uniqId == uniqId)
      id.store = f(id.store.asInstanceOf[T])
      id.didChange = true
      didChanged.append(id)
    }

    override def addCell[T <: CellContent[?, ?]](cell: T): CIdOf[T] = {
      didSomething = true
      val id = new HoldCell[T](uniqId, cell)
      id
    }

    override def addPropagatorGetPid[T <: Propagator[Ops]](
        propagator: T
    )(using more: Ops): PIdOf[T] = {
      given StateOps[Ops] = this
      didSomething = true
      val id = new HoldPropagator[T](uniqId, propagator)
      for (cell <- propagator.defaultingCells)
        cell.zonkingPropagators = cell.zonkingPropagators :+ id.asInstanceOf[PIdOf[Propagator[?]]]
      for (cell <- propagator.readingCells)
        cell.readingPropagators = cell.readingPropagators :+ id.asInstanceOf[PIdOf[Propagator[?]]]
      if (propagator.run(using this, more)) {
        id.alive = false
      }
      id
    }

    override def stable: Boolean = didChanged.isEmpty

    override def tick(using more: Ops): Unit =
      while (didChanged.nonEmpty) {
        val id = didChanged.removeHead()
        if (id.didChange) {
          id.didChange = false
          for (p <- id.readingPropagators) {
            require(p.uniqId == uniqId)
            if (p.alive) {
              if (p.store.asInstanceOf[Propagator[Ops]].run(using this, more)) {
                didSomething = true
                p.alive = false
              }
            }
          }
        }
      }

    var didSomething = false

    override def defaulting(
        cells: Vector[CIdOf[CellContent[?, ?]]]
    )(using more: Ops): Unit = {
      var cellsNeeded = cells
      var tryFallback: Int = 0
      while (true) {
        didSomething = false
        tickAll
        cellsNeeded = cellsNeeded
          .filter(this.noAnyValue)
          .sortBy(x => -x.zonkingPropagators.map(_.store.score).sum)
        if (cellsNeeded.isEmpty) {
          return
        }
        for (c <- cellsNeeded) {
          require(c.uniqId == uniqId)
          if (c.noAnyValue) {
            def processZonking = {
              val aliveP = c.zonkingPropagators.filter(_.alive)
              c.zonkingPropagators = aliveP
              val zonking = aliveP.sortBy(x => -x.store.score)
              zonking
            }
            for (p <- processZonking) {
              require(p.uniqId == uniqId)
              tickAll
              if (c.noAnyValue && p.alive) {
                val store = p.store.asInstanceOf[Propagator[Ops]]
                if (store.run(using this, more)) {
                  p.alive = false
                  didSomething = true
                } else {
                  val on = store.defaulting(cellsNeeded)(using this, more)
                  on match {
                    case DefaultingResult.Done =>
                      p.alive = false
                      didSomething = true
                    case DefaultingResult.Require(needed) =>
                      val needed1 = needed
                        .filter(this.noStableValue)
                        .filterNot(cellsNeeded.contains)
                      if (needed1.nonEmpty) {
                        cellsNeeded = cellsNeeded ++ needed1
                        didSomething = true
                      }
                    case DefaultingResult.NotYet =>
                  }
                }
              }
            }
            if (tryFallback > 0 && !didSomething) {
              for (p <- processZonking) {
                require(p.uniqId == uniqId)
                tickAll
                if (c.noAnyValue && p.alive) {
                  val store = p.store.asInstanceOf[Propagator[Ops]]
                  if (store.run(using this, more)) {
                    p.alive = false
                    didSomething = true
                  } else {
                    val on =
                      store.naiveFallbackZonk(cellsNeeded)(using this, more)
                    on match {
                      case DefaultingResult.Done =>
                        p.alive = false
                        didSomething = true
                      case DefaultingResult.Require(needed) =>
                        val needed1 = needed
                          .filter(this.noStableValue)
                          .filterNot(cellsNeeded.contains)
                        if (needed1.nonEmpty) {
                          cellsNeeded = cellsNeeded ++ needed1
                          didSomething = true
                        }
                      case DefaultingResult.NotYet =>
                    }
                  }
                }
              }
            }
          }
        }
        if (tryFallback > 1 && !didSomething) {
          for (c <- cellsNeeded)
            if (c.noAnyValue && c.store.default.isDefined) {
              fill(c.asInstanceOf[CellId[Any]], c.store.default.get)
              didSomething = true
            }
        }
        if (!didSomething) {
          if (tryFallback > 1) {
            throw new IllegalStateException(
              t"Cells $cellsNeeded are not covered by any propagator"
            )
          } else {
            tryFallback = tryFallback + 1
          }
        } else {
          tryFallback = 0
        }
      }
      tickAll
    }
  }
}
