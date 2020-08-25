// scalac: -Ydelambdafy:method-ref
object Test {
  def min0[A](less: (A, A) => Boolean, xs: List[A]): Option[A] = None

  def min(xs: List[Int]) = min0((x: Int, y: Int) => x < y, xs)

  def main(args: Array[String]): Unit = min(List())
}
