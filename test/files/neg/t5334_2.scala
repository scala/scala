import scala.reflect.mirror._

object Test extends App {
  reify {
    class C { override def toString() = "C" }
    List((new C, new C))
  }.eval
}
