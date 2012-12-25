import Macros._

object Test extends App {
  class D1 extends Foo
  type T = Bar()[Int]
  val x1: Foo = new Foo
  @Foo class D2
}