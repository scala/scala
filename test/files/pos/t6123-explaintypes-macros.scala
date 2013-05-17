import scala.language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  def printf(format: String, params: Any*): Unit = macro printf_impl
  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = ???
}
