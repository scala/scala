class A {
  // First three compile.
  def f1(x: Either[Int, String])        = x.right map (y => y)
  def f2(x: Either[Int, String])        = for (y <- x.right) yield y
  def f3(x: Either[Int, (String, Int)]) = x.right map { case (y1, y2) => (y1, y2) }
  // Last one fails.
  def f4(x: Either[Int, (String, Int)]) = for ((y1, y2) <- x.right) yield ((y1, y2))
/**
./a.scala:5: error: constructor cannot be instantiated to expected type;
 found   : (T1, T2)
 required: Either[Nothing,(String, Int)]
  def f4(x: Either[Int, (String, Int)]) = for ((y1, y2) <- x.right) yield ((y1, y2))
                                               ^
./a.scala:5: error: not found: value y1
  def f4(x: Either[Int, (String, Int)]) = for ((y1, y2) <- x.right) yield ((y1, y2))
                                                                            ^
./a.scala:5: error: not found: value y2
  def f4(x: Either[Int, (String, Int)]) = for ((y1, y2) <- x.right) yield ((y1, y2))
                                                                                ^
three errors found
**/
}
