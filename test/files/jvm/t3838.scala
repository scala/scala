import scala.actors.Actor._

object Test {
  def main(args: Array[String]) {
    actor {
      try {
        receiveWithin(1) {
          case str: String => println(str)
        }
      } catch {
        case e: Exception => println("caught "+e)
      }
    }
  }
}
