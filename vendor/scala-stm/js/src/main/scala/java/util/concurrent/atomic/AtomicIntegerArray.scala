/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.util.concurrent.atomic

class AtomicIntegerArray(val length: Int) extends Serializable {
  type E = Int

  def this(array: Array[Int]) = {
    this(array.length)
    System.arraycopy(array, 0, inner, 0, length)
  }

  private val inner: Array[Int] = new Array[Int](length)

  final def get(i: Int): Int =
    inner(i)

  final def set(i: Int, newValue: E): Unit =
    inner(i) = newValue

  final def lazySet(i: Int, newValue: E): Unit =
    set(i, newValue)

  final def getAndSet(i: Int, newValue: E): E = {
    val ret = get(i)
    set(i, newValue)
    ret
  }

  final def compareAndSet(i: Int, expect: E, update: E): Boolean = {
    if (get(i) != expect) false else {
      set(i, update)
      true
    }
  }

  final def weakCompareAndSet(i: Int, expect: E, update: E): Boolean =
    compareAndSet(i, expect, update)

  override def toString(): String =
    java.util.Arrays.toString(inner)
}
