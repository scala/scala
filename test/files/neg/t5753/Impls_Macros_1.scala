// scalac: -language:experimental.macros
import scala.reflect.macros.blackbox.Context

trait Impls {
  def impl(c: Context)(x: c.Expr[Any]) = x
}

