import scala.actors.{Actor, Exit}
import scala.actors.Actor._

case class MyException(text: String) extends Exception(text) {
  override def fillInStackTrace() = this
}

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
  override def toString = "Master"
  def act() {
    link(Slave)
    Slave ! 'doWork
    react {
      case 'done =>
        throw new MyException("Master crashed")
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
