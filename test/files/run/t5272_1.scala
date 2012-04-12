import scala.reflect.mirror._

object Test extends App {
  reify {
    2 match {
      case 2 => println("okay")
      case _ => println("not okay")
    }
  }.eval
}
