import scala.actors.Futures

object Test {
  def main(args: Array[String]) {
    for (i <- 1 to 100000) {
      Futures.alarm(0)
      if (i % 10000 == 0)
        println("OK")
    }
    for (_ <- 1 to 10) {
      val ft = Futures.alarm(100)
      ft()
      println("OK")
    }
  }
}
