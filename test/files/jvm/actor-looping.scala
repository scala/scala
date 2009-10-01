
import scala.actors.Actor._

object Test {
  case object A

  def main(args: Array[String]) {
    val a = actor {
      var cnt = 0
      loop {
        react {
          case A =>
            cnt += 1
            if (cnt % 2 != 0) continue
            if (cnt < 10)
              println("received A")
            else {
              println("received last A")
              exit()
            }
        }
      }
    }

    for (i <- 0 until 10) a ! A
  }
}
