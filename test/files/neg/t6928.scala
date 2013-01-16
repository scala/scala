abstract class A( val someAs: A* )
object B extends A(B)
object C extends A(null, null, C)

object Test {
  def main(args: Array[String]): Unit = {
    println(B.someAs)
    println(C.someAs)
  }
}
