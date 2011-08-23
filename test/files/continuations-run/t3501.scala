import scala.util.continuations._

object Test {
  def capture(): Int @suspendable = 42

  def main(args: Array[String]): Unit = reset {
    var i = 0
    while (i < 5) {
      i += 1
      val y = capture()
      val s = y
      println(s)
    }
  }
}
