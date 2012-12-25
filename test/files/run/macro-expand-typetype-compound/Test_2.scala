object Test extends App {
  import Macros._
  class C { def x = 2 }
  val x: Foo(2) = new C
}