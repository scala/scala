class A {
  def f5(x: Either[Int, String])        = for ((y1, y2: String) <- x.right) yield ((y1, y2))
  def f6(x: Either[Int, String])        = for ((y1, y2: Any) <- x.right) yield ((y1, y2))
  def f7(x: Either[Int, (String, Int)]) = for (y1 @ Tuple1(y2) <- x.right) yield ((y1, y2))
  def f8(x: Either[Int, (String, Int)]) = for ((y1, y2, y3) <- x.right) yield ((y1, y2))
}
