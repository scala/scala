import scala.concurrent.Actor

object test extends Actor {
  receive {
    case testp.TIMEOUT => Console.println("TIMEOUT")
    //case _       => Console.println("_")
  }
}

