// scalac: -Xsource:3
//
class Foo {
  def unary_~() : Foo = this

  def unary_! : Foo = this // ok
}
object Test {
  val f = new Foo
  val f2 = ~f
}
