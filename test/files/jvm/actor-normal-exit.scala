
import scala.actors.{Actor, Exit}

object Test {
  object Master extends Actor {
    trapExit = true
    def act() {
      try {
      Slave.start()
      react {
        case Exit(from, reason) =>
          println("slave exited for reason " + reason)
      }
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
  }

  object Slave extends Actor {
    def act() {
      try {
      link(Master)
      println("Done")
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
  }

  def main(args: Array[String]) {
    Master.start()
  }
}
