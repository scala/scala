module m1 {
  def factorial(n: Int): Int =
    if (n == 0) 1
    else n * factorial(n-1);
}

module m2 {

  def factorial(n: Int): Int = {
    def factIter(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else factIter(n - 1, acc * n)
    }
    factIter(n, 1)
  }
}
