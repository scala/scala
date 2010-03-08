import scala.actors.{Actor, TIMEOUT}
import Actor._

object Test {
  def main(args: Array[String]) {
    val a = actor {
      react {
        case 'hello =>
          reply(42)
      }
    }

    val b = actor {
      self.trapExit = true
      val ft = a !! 'hello
      println(ft())
      // no message should be left over in mailbox
      reactWithin(0) {
        case TIMEOUT =>
          println("OK")
        case any =>
          println(any)
      }
    }
  }
}
