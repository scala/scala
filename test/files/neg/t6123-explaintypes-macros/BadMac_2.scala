import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

// explain some macro types to me
object BadMac {
  def printf(format: String, params: Any*): Unit = macro printf_impl
  def printf_impl(c: BlackboxContext)(format: c.Expr[String], params: c.Expr[String]*): c.Expr[Unit] = ???
}
