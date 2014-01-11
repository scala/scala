import scala.language.experimental.macros
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}

object Impls {
  def foo(c: BlackboxContext)(x: c.Expr[Int]) = x

  def refToFoo(dummy: Int): Int = macro refToFoo_impl
  def refToFoo_impl(c: WhiteboxContext)(dummy: c.Expr[Int]) = {
    import c.universe._
    val body = Select(Ident(TermName("Impls")), TermName("foo"))
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    global.analyzer.markMacroImplRef(body.asInstanceOf[global.Tree])
    c.Expr[Int](body)
  }
}