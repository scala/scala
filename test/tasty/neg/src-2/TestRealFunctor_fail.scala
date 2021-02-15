package tastytest

object TestRealFunctor {
  // in this test, we try to implement an extension method that has two type-parameter lists
  implicit object ListRealFunctor extends RealFunctor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

}
