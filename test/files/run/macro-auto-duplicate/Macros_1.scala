import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val x = Ident(newTermName("x"))
    def defAndUseX(rhs: Tree) = {
      Block(List(ValDef(NoMods, newTermName("x"), TypeTree(), rhs)), x)
    }
    val xs2 = defAndUseX(Literal(Constant("4")))
    val xi4 = defAndUseX(Literal(Constant(2)))
    c.Expr[String](Apply(Select(xs2, newTermName("$plus")), List(xi4)))
  }

  def foo: String = macro impl
}
