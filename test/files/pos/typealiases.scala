package foo

trait Test[T] {
  type Check[T] = Array[T] => Unit;
  type MyPair[S] = (T, S)

  val pair1: (T, Int)
  val pair: MyPair[Int] = pair1

  def check(xs: Array[T], c: Check[T]) = c(xs)
  def check2[S](xs: Array[S], c: Check[S]) = c(xs)
}

object main extends Test[Int] {
  val pair1 = (1,1)

  implicit def topair(x: Int): Tuple2[Int, Int] = (x,x)
  val pair2: MyPair[Int] = 1
  val x: Short = 1
}
