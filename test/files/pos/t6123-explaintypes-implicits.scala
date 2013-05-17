object ImplicitBugReport {
  trait Exp[+T]
  trait CanBuildExp[-Elem, +To] extends (Exp[Elem] => To)
  trait TraversableExp[T, ExpT <: Exp[T]] extends Exp[Traversable[T]]

  implicit def canBuildExp[T]: CanBuildExp[T, Exp[T]] = ???
  implicit def canBuildExpTrav[T, ExpT <: Exp[T]](implicit c: CanBuildExp[T, ExpT]): CanBuildExp[Traversable[T], TraversableExp[T, ExpT]] = ???
  def toExpTempl[T, That](t: T)(implicit c: CanBuildExp[T, That]): That = ???

  def testBug() {
    val a1 = toExpTempl(Seq(1, 2, 3, 5))
  }
}
