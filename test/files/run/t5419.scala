import scala.reflect.runtime.universe._

class Foo extends annotation.StaticAnnotation

object Test extends App {
  val tree = reify{(5: @Foo).asInstanceOf[Int]}.tree
  println(tree.toString)
}