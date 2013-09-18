trait Iterable[A <: Bound[A], Bound[_]] {
  type MyType[x <: Bound[x]] <: Iterable[x, Bound]
  def map[B <: Bound[B]](f: A => B): MyType[B]
  def flatMap[B <: Bound[B]](f: A => MyType[B]): MyType[B]
  def filter(p: A => Boolean): MyType[A]
}

trait OrderedSet[T <: Ordered[T]] extends Iterable[T, Ordered] {
  type MyType[x <: Ordered[x]] = OrderedSet[x]
}
