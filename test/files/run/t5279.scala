import scala.reflect.mirror._

object Test extends App {
  reify {
    println(new Integer(10))
  }.eval
}
