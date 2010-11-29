package scala.collection.generic

/** A template trait that contains just the `map`, `flatMap`, `foreach` and `withFilter` methods
 *  of trait `TraversableLike`.
 */
trait FilterMonadic[+A, +Repr] {
  def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That
  def flatMap[B, That](f: A => TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That
  def foreach[U](f: A => U): Unit
  def withFilter(p: A => Boolean): FilterMonadic[A, Repr]
}
