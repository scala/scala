import scala.language.experimental.macros
import scala.reflect.macros.Context

// explain some macro types to me
object BadMac {
  def printf(format: String, params: Any*): Unit = macro printf_impl
  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[String]*): c.Expr[Unit] = ???
}
