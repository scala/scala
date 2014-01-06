

@deprecated("Suppress warnings", since="2.11")
object Test {
  import scala.actors.Actor._
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
