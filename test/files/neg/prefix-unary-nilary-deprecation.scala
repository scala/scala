//> using options -Werror -Xlint:deprecation
//
class Foo {
  def unary_~() : Foo = this
  def unary_-()(implicit pos: Long) = this

  def unary_! : Foo = this // ok
  def unary_+(implicit pos: Long) = this // ok
}
object Test {
  val f = new Foo
  val f2 = ~f
}
