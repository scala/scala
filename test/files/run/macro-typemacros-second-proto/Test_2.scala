import language.experimental.macros

object Test extends App {
  type Foo[T](name: String) = macro Macros.impl[T]
  val x: Foo[Int]("2") = 2

  type Foo2 = macro Macros.impl2
  val y: Foo2 = 3

  type Foo3 = Foo2
  val z: Foo3 = 4
}