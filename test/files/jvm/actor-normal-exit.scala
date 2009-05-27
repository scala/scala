
import scala.actors.{Actor, Exit}

object Test {
  object Master extends Actor {
    trapExit = true
    def act() {
      Slave.start()
      react {
        case Exit(from, reason) =>
          println("slave exited for reason " + reason)
      }
    }
  }

  object Slave extends Actor {
    def act() {
      link(Master)
      println("Done")
    }
  }

  def main(args: Array[String]) {
    Master.start()
  }
}
