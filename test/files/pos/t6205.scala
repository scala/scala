// original code by reporter
class A[T]
class Test1 {
  def x(backing: Map[A[_], Any]) =
    for( (k: A[kt], v) <- backing)
      yield (k: A[kt])
}

// this tests same thing as above, but independent of library classes,
// earlier expansions eliminated as well as variance (everything's invariant)
case class Holder[A](a: A)
class Mapped[A] { def map[T](f: Holder[A] => T): Iterable[T] = ??? }
class Test2 {
  def works(backing: Mapped[A[_]]): Iterable[A[_]]
    = backing.map(x =>
         x match {case Holder(k: A[kt]) => (k: A[kt])}
      )
}