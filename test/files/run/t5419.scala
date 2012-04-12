import scala.reflect.mirror._

class Foo extends StaticAnnotation

object Test extends App {
  val tree = reify{(5: @Foo).asInstanceOf[Int]}.tree
  println(tree.toString)
}