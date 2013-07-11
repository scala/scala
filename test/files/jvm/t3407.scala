

@deprecated("Suppress warnings", since="2.11")
object Test {
  import scala.actors._, scala.actors.Actor._

  def main(args: Array[String]) {
    for (i <- 1 to 10) {
      val ft = Futures.future { 42 }
      println("result: " + ft())
    }

    for (i <- 1 to 10) {
      receiveWithin(0) {
        case TIMEOUT =>
        case msg => println("unexpected: " + msg)
      }
    }
  }

}
