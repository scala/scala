import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def printf(format: String, params: Any*): Unit = macro printf_impl
  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = ???
}

// something trivial to run
object Test extends App
