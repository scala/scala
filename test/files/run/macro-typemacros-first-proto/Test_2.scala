import Macros._

class C extends AnyRef with Foo[Int]("2")

object Test extends App {
  println(new C().hello)
}
