import scala.util.continuations._

object Test extends App {
  def foo(x:Int): Int @cps[Int] = shift { k => k(x) }

  // should be a type error
  def bar(x:Int): Int @cps[String] = return foo(x)

  def caller(): Unit = {
    val v: String = reset {
      val res: Int = bar(8)
      "hello"
    }
  }

  caller()
}
