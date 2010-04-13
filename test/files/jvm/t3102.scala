import scala.actors.{Actor, TIMEOUT}
import Actor._

object Test {
  def main(args: Array[String]) {
    val a = actor {
      try {
      react {
        case 'hello =>
          reply(42)
      }
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }

    val b = actor {
      try {
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
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
  }
}
