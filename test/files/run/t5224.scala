import scala.reflect.runtime.universe._

class Foo(bar: String) extends annotation.ClassfileAnnotation

object Test extends App {
  val tree = reify{@Foo(bar = "qwe") class C}.tree
  println(tree.toString)
}