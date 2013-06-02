import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = x

  def refToFoo(dummy: Int) = macro refToFoo_impl
  def refToFoo_impl(c: Ctx)(dummy: c.Expr[Int]) = {
    import c.universe._
    val body = Select(Ident(TermName("Impls")), TermName("foo"))
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    global.analyzer.markMacroImplRef(body.asInstanceOf[global.Tree])
    c.Expr[Int](body)
  }
}