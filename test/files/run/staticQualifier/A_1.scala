import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object A {
  def foo(x: Int): Int = macro foo_impl

  def foo_impl(c: Context)(x: c.Expr[Int]): c.Tree = {
    val g = c.universe.asInstanceOf[scala.tools.nsc.Global]
    import g._
    import scala.tools.nsc.symtab.Flags._

    val t = x.tree.asInstanceOf[Tree] match {
      case s @ Select(_, n) if n.toString == "f2" =>
        val field = s.symbol.accessed
        field.setFlag(STATIC).resetFlag(PRIVATE | LOCAL)
        s.setSymbol(field)
    }

    t.asInstanceOf[c.Tree]
  }
}
