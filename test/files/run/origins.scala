import scala.tools.nsc.util.Origins

package goxbox {
  object Socks {
    val origins = Origins[Socks.type]("boop")

    def boop(x: Int): Int = origins { 5 }
  }
}

object Test {
  import goxbox.Socks.boop

  def f1() = 1 to 5 map boop
  def f2() = 1 to 10 map boop
  def f3() = 1 to 50 map boop
  
  def main(args: Array[String]): Unit = {
    f1() ; f2() ; f3()
  }
}
