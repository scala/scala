import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context): c.Expr[Any] = {
    import c.universe._;

    val res0 = c.eval(c.Expr[Any](q"implicitly[ValueOf[1]]"))
    val res = res0.asInstanceOf[ValueOf[1]].value

    c.Expr[Any](q"$res")
  }

  def eval: Any = macro impl
}
