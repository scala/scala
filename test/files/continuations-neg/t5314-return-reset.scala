import scala.util.continuations._
import scala.util.Random

object Test extends App {
  val rnd = new Random

  def foo(x: Int): Int @cps[Int] = shift { k => k(x) }

  def bar(x: Int): Int @cps[Int] = return foo(x)

  def caller(): Int = {
    val v: Int = reset {
      val res: Int = bar(8)
      if (rnd.nextInt(100) > 50) return 5 // not allowed, since method is calling `reset`
      42
    }
    v
  }

  caller()
}
