import scala.reflect.mirror._

object Test extends App {
  reify {
    implicit lazy val x = 2
    println(implicitly[Int])
  }.eval
}
