import scala.reflect.mirror._

object Test extends App {
  reify {
    lazy val x = 2
    println(x)
  }.eval
}
