import scala.reflect.mirror._

object Test extends App {
  reify {
    class C
    println(classOf[C])
  }.eval
}