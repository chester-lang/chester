/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package std

import spire.algebra._

@SerialVersionUID(0L)
class MapMonoid[K, V](implicit val scalar: Semigroup[V]) extends Monoid[Map[K, V]] with Serializable {
  def empty: Map[K, V] = Map.empty

  def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    var xx = x
    var yy = y
    var f = scalar.combine
    if (x.size < y.size) { xx = y; yy = x; f = (x: V, y: V) => scalar.combine(y, x) }
    yy.foldLeft(xx) { (z, kv) =>
      z.updated(kv._1, xx.get(kv._1).map(u => f(u, kv._2)).getOrElse(kv._2))
    }
  }
}

@SerialVersionUID(0L)
class MapGroup[K, V](implicit override val scalar: Group[V])
    extends MapMonoid[K, V]
    with Group[Map[K, V]]
    with Serializable {
  def inverse(x: Map[K, V]): Map[K, V] = x.view.mapValues(scalar.inverse).toMap
}

@SerialVersionUID(0L)
class MapCSemiring[K, V](implicit val scalar: CSemiring[V]) extends CSemiring[Map[K, V]] with Serializable {

  def zero: Map[K, V] = Map.empty

  def plus(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    var xx = x
    var yy = y
    if (x.size < y.size) { xx = y; yy = x }
    yy.foldLeft(xx) { (z, kv) =>
      z.updated(kv._1, xx.get(kv._1).map(u => scalar.plus(u, kv._2)).getOrElse(kv._2))
    }
  }

  def times(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    var xx = x
    var yy = y
    var f = scalar.times
    if (x.size < y.size) { xx = y; yy = x; f = (x: V, y: V) => scalar.times(y, x) }
    yy.foldLeft(zero) { (z, kv) =>
      xx.get(kv._1).map(u => z.updated(kv._1, f(u, kv._2))).getOrElse(z)
    }
  }
}

@SerialVersionUID(0L)
class MapCRng[K, V](implicit override val scalar: CRing[V])
    extends MapCSemiring[K, V]
    with CRng[Map[K, V]]
    with CModule[Map[K, V], V]
    with Serializable { self =>
  // TODO: in the experimental branch, it is no longer an algebra, because it lacks an identity

  def negate(x: Map[K, V]): Map[K, V] = x.view.mapValues(scalar.negate(_)).toMap

  def timesl(r: V, v: Map[K, V]): Map[K, V] = v.view.mapValues(scalar.times(r, _)).toMap
}

@SerialVersionUID(0L)
class MapVectorSpace[K, V](implicit override val scalar: Field[V])
    extends MapCRng[K, V]
    with VectorSpace[Map[K, V], V]
    with Serializable {
  override def times(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    var xx = x
    var yy = y
    if (x.size < y.size) { xx = y; yy = x }
    yy.foldLeft(zero) { (z, kv) =>
      xx.get(kv._1).map(u => z.updated(kv._1, scalar.times(u, kv._2))).getOrElse(z)
    }
  }
}

@SerialVersionUID(0L)
class MapInnerProductSpace[K, V: Field]
    extends MapVectorSpace[K, V]
    with InnerProductSpace[Map[K, V], V]
    with Serializable {
  def dot(x: Map[K, V], y: Map[K, V]): V =
    times(x, y).foldLeft(scalar.zero) { (a, b) => scalar.plus(a, b._2) }
}

@SerialVersionUID(0L)
class MapEq[K, V](implicit V: Eq[V]) extends Eq[Map[K, V]] with Serializable {
  def eqv(x: Map[K, V], y: Map[K, V]): Boolean = {
    if (x.size != y.size) false
    else {
      x.forall { case (k, v) =>
        y.get(k) match {
          case Some(e) if V.eqv(e, v) => true
          case _                      => false
        }
      }
    }
  }
}

@SerialVersionUID(0L)
class MapVectorEq[K, V](implicit V: Eq[V], scalar: AdditiveMonoid[V]) extends Eq[Map[K, V]] with Serializable {
  def eqv(x: Map[K, V], y: Map[K, V]): Boolean = {
    @tailrec
    def loop(acc: Map[K, V], it: Iterator[(K, V)]): Boolean = {
      if (it.hasNext) {
        val (k, v0) = it.next()
        acc.get(k) match {
          case Some(v1) if V.eqv(v0, v1) =>
            loop(acc - k, it)
          case None if V.eqv(v0, scalar.zero) =>
            loop(acc - k, it)
          case _ =>
            false
        }
      } else {
        acc.forall { case (_, v) => V.eqv(v, scalar.zero) }
      }
    }

    loop(x, y.iterator)
  }
}

trait MapInstances0 {
  implicit def MapMonoid[K, V: Semigroup]: MapMonoid[K, V] = new MapMonoid[K, V]

  implicit def MapCSemiring[K, V: CSemiring]: MapCSemiring[K, V] = new MapCSemiring[K, V]
}

trait MapInstances1 extends MapInstances0 {
  implicit def MapCRng[K, V: CRing]: MapCRng[K, V] = new MapCRng[K, V]
}

trait MapInstances2 extends MapInstances1 {
  implicit def MapGroup[K, V: Group]: MapGroup[K, V] = new MapGroup[K, V]

  implicit def MapVectorSpace[K, V: Field]: MapVectorSpace[K, V] = new MapVectorSpace[K, V]
}

trait MapInstances3 extends MapInstances2 {
  implicit def MapInnerProductSpace[K, V: Field]: MapInnerProductSpace[K, V] = new MapInnerProductSpace[K, V]

  implicit def MapEq[K, V](implicit V0: Eq[V]): MapEq[K, V] = new MapEq[K, V]
}

trait MapInstances extends MapInstances3
