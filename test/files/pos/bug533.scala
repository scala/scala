import scala.actors._

object test extends Actor {
  def act() {
    receive {
      case TIMEOUT => Console.println("TIMEOUT")
      //case _       => Console.println("_")
    }
  }
}

