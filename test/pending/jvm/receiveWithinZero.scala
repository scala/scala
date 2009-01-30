import scala.actors.{Actor, TIMEOUT}

class A extends Actor {
  def act() = receiveWithin(0) {
    case TIMEOUT =>
      println("TIMEOUT")
      reply('ack)
      act()
    case x => println(x)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val a = new A
    a.start()
  }
}
