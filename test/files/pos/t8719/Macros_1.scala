import scala.language.experimental.macros
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.whitebox.Context

object Macros {
  def typecheck_impl(c: Context)(code: c.Expr[String]): c.Expr[Option[String]] = {
    import c.universe._

    val Expr(Literal(Constant(codeStr: String))) = code

    try {
      c.typecheck(c.parse(codeStr))
      c.Expr(q"None: Option[String]")
    } catch {
      case e: TypecheckException =>
        c.Expr(q"Some(${ e.toString }): Option[String]")
    }
  }

  def typecheck(code: String): Option[String] = macro typecheck_impl
}