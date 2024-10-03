package chester.utils.propagator

import chester.syntax.core.{UniqId, UniqIdOf}

import scala.collection.mutable

trait ProvideMutable extends ProvideImpl {
  class HoldCell[+T <:Cell[?]](val uniqId: UniqIdOf[Impl[?]], value: T) {
    var store: Cell[?] = value
    var didChange: Boolean = false
    var readingPropagators: Vector[PIdOf[Propagator[?]]] = Vector.empty
    var zonkingPropagators: Vector[PIdOf[Propagator[?]]] = Vector.empty
    inline def noValue: Boolean = store.noValue
  }
  type CIdOf[+T <:Cell[?]] = HoldCell[T]
  class HoldPropagator[+T<:Propagator[?]](val uniqId: UniqIdOf[Impl[?]], value: T) {
    var store: Propagator[?] = value
    var alive: Boolean = true
  }
  type PIdOf[+T<:Propagator[?]] = HoldPropagator[T]
  override def isCId(x: Any): Boolean = {
    val result = x.isInstanceOf[HoldCell[?]]
    result
  }
  override def assumeCId(x: Any): CIdOf[Cell[?]] = {
    require(isCId(x))
    x.asInstanceOf[CIdOf[Cell[?]]]
  }

  override def stateAbilityImpl[Ability]: StateAbility[Ability] = Impl[Ability]()
  class Impl[Ability](val uniqId: UniqIdOf[Impl[Ability]] = UniqId.generate[Impl[Ability]]) extends StateAbility[Ability] {
    var didChanged: mutable.ArrayDeque[CIdOf[?]] = mutable.ArrayDeque.empty
    override def readCell[T <: Cell[?]](id: CIdOf[T]): Option[T] = {
      require(id.uniqId == uniqId)
      Some(id.store.asInstanceOf[T])
    }
    override def update[T <: Cell[?]](id: CIdOf[T], f: T => T): Unit = {
      require(id.uniqId == uniqId)
      id.store = f(id.store.asInstanceOf[T])
      id.didChange = true
      didChanged.append(id)
    }
    override def addCell[T <: Cell[?]](cell: T): CIdOf[T] = {
      val id = new HoldCell[T](uniqId, cell)
      id
    }
    override def addPropagator[T<:Propagator[Ability]](propagator: T)(using more: Ability): PIdOf[T] = {
      val id = new HoldPropagator[T](uniqId, propagator)
      for(cell <- propagator.zonkingCells) {
        cell.zonkingPropagators = cell.zonkingPropagators :+ id.asInstanceOf[PIdOf[Propagator[?]]]
      }
      for(cell <- propagator.readingCells) {
        cell.readingPropagators = cell.readingPropagators :+ id.asInstanceOf[PIdOf[Propagator[?]]]
      }
      id
    }
    override def stable: Boolean = didChanged.isEmpty
    override def tick(using more: Ability): Unit = {
      while (didChanged.nonEmpty) {
        val id = didChanged.removeHead()
        if(id.didChange) {
          id.didChange = false
          for(p <- id.readingPropagators) {
            require(p.uniqId == uniqId)
            if(p.alive) {
              if(p.store.asInstanceOf[Propagator[Ability]].run(using this, more)){
                p.alive = false
              }
            }
          }
        }
      }
    }
    override def readingZonkings(cells: Vector[CIdOf[Cell[?]]]): Vector[Propagator[Ability]] = {
      cells.flatMap(_.zonkingPropagators).map(_.store.asInstanceOf[Propagator[Ability]])
    }
    override def naiveZonk(cells: Vector[CIdOf[Cell[?]]])(using more: Ability): Unit = {
      var cellsNeeded = cells
      while(true) {
        tickAll
        cellsNeeded = cellsNeeded.filter(this.noValue(_))
        if(cellsNeeded.isEmpty) {
          return
        }
        var didSomething = false
        for(c <- cellsNeeded) {
          require(c.uniqId == uniqId)
          for(p <- c.zonkingPropagators) {
            require(p.uniqId == uniqId)
            tickAll
            if(c.noValue && p.alive) {
              p.store.asInstanceOf[Propagator[Ability]].naiveZonk(cellsNeeded)(using this, more) match {
                case ZonkResult.Done =>
                  p.alive = false
                  didSomething = true
                case ZonkResult.Require(needed) =>
                  val needed1 = needed.filter(this.noValue(_)).filterNot(cellsNeeded.contains)
                  if(needed1.nonEmpty) {
                    cellsNeeded = cellsNeeded ++ needed1
                    didSomething = true
                  }
                case ZonkResult.NotYet =>
              }
            }
          }
        }
        if(!didSomething) {
          throw new IllegalStateException(s"Cells $cellsNeeded are not covered by any propagator")
        }
      }
    }
  }
}
