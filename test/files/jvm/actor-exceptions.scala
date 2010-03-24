
import scala.actors.{Actor, Exit}
import Actor._

case class MyException(text: String) extends Exception {
  override def fillInStackTrace() = this
}

case class MyOtherException(text: String) extends Exception {
  override def fillInStackTrace() = this
}

object Master extends Actor {
  trapExit = true
  def act() {
    link(Slave)
    Slave.start()
    for (i <- 0 until 10) Slave ! A
    react {
      case Exit(from, reason) =>
    }
  }
}

object Slave extends Actor {
  override def toString = "Slave"
  override def exceptionHandler: PartialFunction[Exception, Unit] = {
    case MyException(text) =>
  }
  def act() {
    var cnt = 0
    loop {
      react {
        case A =>
          cnt += 1
          if (cnt % 2 != 0) throw MyException("problem")
          if (cnt == 10) {
            throw new MyOtherException("unhandled")
          }
      }
    }
  }
}

case object A

object Test {
  def main(args: Array[String]) {
    Master.start()
  }
}
