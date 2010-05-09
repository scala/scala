import scala.actors._, scala.actors.Actor._

object Test {

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
