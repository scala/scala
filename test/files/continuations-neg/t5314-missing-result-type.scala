import scala.util.continuations._

object Test extends App {
  def foo(x:Int): Int @cps[Int] = x

  def bar(x:Int) = return foo(x)

  reset {
    val res = bar(8)
    println(res)
    res
  }
}
