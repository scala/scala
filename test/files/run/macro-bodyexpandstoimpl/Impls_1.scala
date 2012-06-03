import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = x

  def refToFoo(dummy: Int) = macro refToFoo_impl
  def refToFoo_impl(c: Ctx)(dummy: c.Expr[Int]) = {
    import c.universe._
    val body = Select(Ident(newTermName("Impls")), newTermName("foo"))
    c.Expr[Int](body)
  }
}