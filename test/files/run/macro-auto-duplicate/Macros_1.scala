import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val x = Ident(newTermName("x"))
    def defAndUseX(rhs: Tree) = {
      Block(List(ValDef(NoMods, newTermName("x"), TypeTree(), rhs)), x)
    }
    val xi4 = defAndUseX(Literal(Constant(4)))
    val xs2 = defAndUseX(Literal(Constant("2")))
    c.Expr[String](Apply(Select(xi4, newTermName("$plus")), List(xs2)))
  }

  def foo = macro impl
}