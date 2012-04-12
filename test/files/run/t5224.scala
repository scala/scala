import scala.reflect.mirror._

class Foo(bar: String) extends ClassfileAnnotation

object Test extends App {
  val tree = reify{@Foo(bar = "qwe") class C}.tree
  println(tree.toString)
}