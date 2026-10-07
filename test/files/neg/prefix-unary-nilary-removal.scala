//> using options -Werror -Xlint
//
class Foo {
  def unary_~(): Foo = Foo()
  def unary_-()(implicit pos: Long) = Foo()

  def `unary_!`: Foo = Foo() // ok
  def unary_+(implicit pos: Long) = Foo() // ok
}
object Foo {
  def apply() = new Foo
}
object Test {
  val f = Foo()
  val f2 = ~f
}
