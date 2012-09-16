import language.dynamics

object Test extends App {
  val x = new X(3)
  val y = x(9)
  class X(i: Int) extends Dynamic {
    def applyDynamic(name: String)(in: Int): Int = {
      println(s"applyDynamic($name)($in)")
      i + in
    }
  }
}
