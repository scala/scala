import scala.actors.{Actor, Exit}

class MyException(msg: String) extends Exception(msg) {
  override def fillInStackTrace() = this
}

object Test {

  case object StartError extends Actor {
    def act() {
      throw new MyException("I don't want to run!")
    }
  }

  case object MessageError extends Actor {
    def act() {
      react {
        case _ => throw new MyException("No message for me!")
      }
    }
  }

  case object Supervisor extends Actor {
    def act() {
      trapExit = true
      link(StartError)
      link(MessageError)
      StartError.start()
      MessageError.start()

      Actor.loop {
        react {
          case Exit(actor, reason) =>
            println(actor + " exited for reason " + reason)
            if (actor == StartError)
              MessageError ! 'ping
            else
              exit()
        }
      }
    }
  }

  def main(args: Array[String]) {
    Supervisor.start()
  }
}
