//> using options -Xsource:3

object Test extends App {
  Foo.ctx.quote(42).ast.id
}
// was: Test_2.scala:4: error: value id is not a member of Ast
