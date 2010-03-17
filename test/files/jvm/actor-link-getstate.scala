import scala.actors.{Actor, Exit}
import scala.actors.Actor._

object Slave extends Actor {
  def act() {
    loop {
      react {
        case 'doWork =>
          println("Done")
          reply('done)
      }
    }
  }
}

object Master extends Actor {
  def act() {
    link(Slave)
    Slave ! 'doWork
    react {
      case 'done =>
        throw new Exception("Master crashed")
    }
  }
}

object Test {

  def main(args: Array[String]) {
    actor {
      self.trapExit = true
      link(Slave)
      Slave.start()
      Master.start()
      react {
        case Exit(from, reason) if (from == Slave) =>
          println(Slave.getState)
      }
    }
  }

}
