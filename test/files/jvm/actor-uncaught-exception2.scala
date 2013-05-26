@deprecated("Suppress warnings", since="2.11")
object Test {
import scala.actors.{Actor, Exit, Debug}

class MyException(msg: String) extends Exception(msg) {
  override def fillInStackTrace() = this
}

  case object StartError extends Actor { 
    def act() { 
      try {
      throw new MyException("I don't want to run!") 
      } catch {
        case e: Throwable if (!e.isInstanceOf[scala.util.control.ControlThrowable] &&
                              !e.isInstanceOf[MyException]) =>
          e.printStackTrace()
      }
    } 
  } 

  case object MessageError extends Actor { 
    def act() { 
      try {
      react { 
        case _ => throw new MyException("No message for me!") 
      } 
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    } 
  } 

  case object Supervisor extends Actor { 
    def act() { 
      try {
      trapExit = true 
      link(StartError)
      link(MessageError)
      StartError.start() 
      MessageError.start()

      Actor.loop { 
        react { 
          case Exit(actor, reason) =>
            println(reason)
            if (actor == StartError)
              MessageError ! 'ping
            else
              exit()
        } 
      } 
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    } 
  } 

  def main(args: Array[String]) { 
    Supervisor.start() 
  } 
} 
