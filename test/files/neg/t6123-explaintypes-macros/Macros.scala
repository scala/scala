import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

object Macros {
  def printf(format: String, params: Any*): Unit = macro printf_impl
  def printf_impl(c: BlackboxContext)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = ???
}

// something trivial to run
object Test extends App
