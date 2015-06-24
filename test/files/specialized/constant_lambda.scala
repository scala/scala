// during development of late delambdafying there was a problem where
// specialization would undo some of the work done in uncurry if the body of the
// lambda had a constant type. That would result in a compiler crash as
// when the delambdafy phase got a tree shape it didn't understand
class X[@specialized(Int) A] {
  val f = { x: A => false }
}

object Test {
  def main(args: Array[String]) {
    val xInt = new X[Int]
    println(xInt.f(42))
    val xString = new X[String]
    println(xString.f("hello"))
  }
}