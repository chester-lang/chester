# Chester Language - Trying to make a practical possibly unsound object-oriented functional dependently typed language with algebraic effects

It might look like?

```chester
trait 舞 <: Show;

@derive(Show)
record 超会議 <: 舞 {
  let year: Nat;
}

@derive(Show)
object InternetOverdose <: 舞;

module 超会議 {
  let バタフライ_グラフィティ: 舞 = 超会議(2017);
}
i: InternetOverdose.type = InternetOverdose;

sealed trait Expr[T: Type] {
  def eval: T;
}

record IVal <: Expr[Int] {
  let val: Int;
  override def eval = val;
}

record BVal <: Expr[Int] {
  let val: Bool;
  override def eval = val;
}

sealed trait Vect[n: Nat, T: Type] {
  def apply(index: Fin n): T = ?todo;
}

object Nil[T] <: Vect[0, T];
record Cons[n,T] <: Vect[n+1, T] {
  let head: T;
  let tail: Vect[n, T];
}
n: Nil.type[Int] = Nil;

proof1[T]: Nil[T] = Nil[T];
proof1[T] = ?hole;

record MutableString {
  var name: String;
}

record MutableStringExplicit[a: STScope] {
  var[a] name: String;
}

// IO somehow gives an implicit STScope?
entry: Unit / IO = {
  let a = MutableString("");
  a.name = "はっぱ - もうすぐ楽になるからね";
  println(a.name);
}
```
