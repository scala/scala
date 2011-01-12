import scala.actors.{Actor, Exit}
import scala.actors.Actor._

case class MyException(text: String) extends Exception(text) {
  override def fillInStackTrace() = this
}

object Slave extends Actor {
  def act() {
    try {
    loop {
      react {
        case 'doWork =>
          Console.out.println("Done")
          reply('done)
      }
    }
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}

object Master extends Actor {
  override def toString = "Master"
  def act() {
    try {
    link(Slave)
    Slave ! 'doWork
    react {
      case 'done =>
        throw new MyException("Master crashed")
    }
    } catch {
      case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
        e.printStackTrace()
    }
  }
}

object Test {

  def main(args: Array[String]) {
    actor {
      try {
      self.trapExit = true
      link(Slave)
      Slave.start()
      Master.start()
      react {
        case Exit(from, reason) if (from == Slave) =>
          Console.out.println(Slave.getState)
      }
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
  }

}
