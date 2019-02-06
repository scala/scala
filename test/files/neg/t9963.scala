object t9963 {
  class MyIterable[+A] {
    def flatMap[B](f: A => MyIterable[B]): MyIterable[B] = ???
    def map[B](f: A => B): MyIterable[B] = ???
  }

  class MySet[A] {
    def map[B: Equiv](f: A => B): MySet[B] = ??? // must have an implicit typeclass here to trigger this bug
    def filter(f: A => Boolean): MySet[A] = ???
  }

  def f[A] = for {
    i <- new MyIterable[A]()
    j: A <- new MySet[A]() // must have a typecheck patmat here to trigger this bug
  } yield (i, j)
}
