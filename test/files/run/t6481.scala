abstract class foo(a: Int, b: Int) extends scala.DelayedInit {
  def delayedInit(x: => Unit): Unit = {
    println("delayed init");
    x
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new foo(1, 2) { println("new foo(1, 2)") }
    new foo(b = 2, a = 1) { println("new foo(b = 2, a = 1)") }
  }
}
