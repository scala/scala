import scala.concurrent._

object test extends Actor {
  receive {
    case TIMEOUT => Console.println("TIMEOUT")
    //case _       => Console.println("_")
  }
}

