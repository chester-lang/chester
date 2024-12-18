package chester.syntax

import chester.utils.reuse

trait Tree[A <: Tree[A]] extends Any {
  type RootTree = A
  type ThisTree <: Tree[A]
  // it is too hard to verify following properties in scala type system, so we just assume them
  implicit val ev1: Tree[A] <:< A = implicitly[A =:= A].asInstanceOf[Tree[A] <:< A]
  given ev3[T <: A](using x: T): (x.ThisTree <:< A) = implicitly[A <:< A].asInstanceOf[x.ThisTree <:< A]

  // this utility method is not that type safe
  protected final inline def thisOr[T <: A](inline x: T): T =
    reuse(this.asInstanceOf[T], x)

  def descent(f: A => A, g: TreeMap[A]): A
  final def descent(f: A => A): A = descent(
    f,
    new TreeMap[A] {
      def use[T <: A](x: T): x.ThisTree = x.descent(f.asInstanceOf).asInstanceOf[x.ThisTree]
    }
  )
  final def descent2(f: TreeMap[A]): ThisTree = descent(
    { x =>
      implicit val ev33: x.ThisTree <:< A = ev3(using x)
      f.use(x)
    },
    f
  ).asInstanceOf[ThisTree]

  final def descentRecursive(f: A => A): A = thisOr {
    f(descent { a =>
      a.descentRecursive(f(_))
    })
  }

  def inspect(f: A => Unit): Unit = {
    val _ = descent2(new TreeMap[A] {
      def use[T <: A](x: T): x.ThisTree = { f(x); x.asInstanceOf[x.ThisTree] }
    })
    ()
  }

  def inspectRecursive(operator: A => Unit): Unit = {
    inspect { a =>
      a.inspectRecursive(operator(_))
    }
    operator(this.asInstanceOf[A])
  }

}

/** means not changing the subtype of Term */
trait TreeMap[Tre <: Tree[Tre]] {

  /** note that special rules might apply when x is MetaTerm, which is hard to represent in scala type system */
  def use[T <: Tre](x: T): x.ThisTree
  final def apply[T <: Tre](x: T): T = use(x).asInstanceOf[T]
}

implicit def convertSpecialMap2[A <: Tree[A], T <: A](f: TreeMap[A]): Tree[A] => T = x => f.use(x.asInstanceOf[T]).asInstanceOf[T]
implicit def conversion1[A <: Tree[A], T <: Tree[A], U <: T](x: Vector[T]): Vector[U] = x.asInstanceOf[Vector[U]]
implicit def conversion11[A <: Tree[A], T <: Tree[A], U <: T](x: T): U = x.asInstanceOf[U]
//implicit  def conversion2[A <: Tree[A], T <: Tree[A]](x:T): A&T = x.asInstanceOf[A&T]
