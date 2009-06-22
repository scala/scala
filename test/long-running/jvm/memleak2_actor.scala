import scala.actors._
import Actor._

case object Start
case object EndMe

class A extends Actor {
  def act = loop {
    react {
      case Start =>
      case EndMe =>
        exit()
    }
  }
}

object Test {

  def z(in: Long) = if (in / 1024L == 0L) in
                    else if (in / (1024L * 1024L) == 0L) (in / 1024L).toString + "K"
                    else (in / (1024L * 1024L)).toString + "M"

  def main(args: Array[String]) {
    val rt = Runtime.getRuntime()
    for (o <- 1 to 300000) {
      println("Outer [2AN] "+o)
      var a: List[A] = Nil
      for (i <- 1 to 10000) {
        var t = new A
        a = t :: a
        t.start
        t ! Start
      }
      for (act <- a) act ! EndMe
      //rt.gc()
      println("Free "+z(rt.freeMemory())+" total "+z(rt.totalMemory()))
    }
  }
}
