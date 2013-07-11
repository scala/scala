object Test {
  def fold[A, B](f: (A, => B) => B) = ???
  def f[A, B](x: A, y: B): B = ???
  def bip[A, B] = fold[A, B]((x, y) => f(x, y))
  def bop[A, B] = fold[A, B](f)

  // these work:
  fold[Int, Int]((x, y) => f(x, y))
  fold[Int, Int](f)
}

