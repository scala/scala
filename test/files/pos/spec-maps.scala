trait Fun1[@specialized +R, @specialized -T] {
  def apply(x: T): R
}

object Main {
  def mapA[@specialized B](xs: Array[B], f: Fun1[B, B]) {
    for (i <- 0 until xs.length)
      xs(i) = f(xs(i))
  }
}
