import Macros._

object Test extends App {
  class C1 extends Foo // 1
  // type T2 = FooHK()[Int] // 2
  // val x3: Foo = null // 3
  val x4 = new Foo // 1
  @Foo class C5 // 1
  println(timesExpanded)
}