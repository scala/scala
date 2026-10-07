//> using options -Werror

object Test extends App {
  type A[X] = scala.Array[X]
  def test(x: Any) =
    x match {
      case _: A[A[Int]]     => println("int") // nowarn
      case _: A[A[String]]  => println("str")
      case _: A[A[AnyRef]]  => println("yes")
      case _: Array[String] => println("arr")
      case _                => println("no")
    }
  test(new A[A[String]](0))
  test(new A[String](0))
}
