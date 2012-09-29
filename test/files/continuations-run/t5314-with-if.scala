import scala.util.continuations._

object Test extends App {

  def foo(x:Int): Int @cps[Int] = 7

  def bar(x:Int): Int @cps[Int] = {
    val v = foo(x)
    if (v > 0)
      return v
    else
      return 10
  }

  println(reset { bar(10) })

}
