import scala.reflect.mirror._

object Test extends App {
  reify {
    2 match {
      case x => println("okay" + x)
    }
  }.eval
}
