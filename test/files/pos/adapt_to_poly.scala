trait Iterable[+T]

trait O[T] {
  def flatMap[U](f: T => O[U]): O[U] = ???
  def foo[U <: String](t: U) = "C#foo"
}
class MapOptionFlatMapIterable[A](o: O[A]) {
  def flatMap[B](f: A => Iterable[B]): Iterable[B] = ???
  def foo(x: Any) = "D#foo"
}

// Monomorphic version worked before the fix:
// trait O[T] { def flatMap(f: T => O[Int]): O[Int] = ??? }
// class MapOptionFlatMapIterable[A](o: O[A]) { def flatMap(f: A => Iterable[Int]): Iterable[Int] = ??? }

class Test {
  implicit def mofmi[A](oo: O[A]): MapOptionFlatMapIterable[A] = new MapOptionFlatMapIterable[A](oo)

  val f: Int => Iterable[Int] = ???

  (??? : O[Int]).flatMap(f)

  (??? : O[Int]).foo(0)
}
