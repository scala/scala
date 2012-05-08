import scala.reflect.mirror._

object Test extends App {
  reify {
    println(classOf[Int])
  }.eval
}