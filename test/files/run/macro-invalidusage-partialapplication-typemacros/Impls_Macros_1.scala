import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int])(y: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("Int"))
}

object Macros {
  type Foo(x: Int)(y: Int) = macro Impls.foo
}