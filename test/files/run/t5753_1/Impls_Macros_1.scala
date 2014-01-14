import scala.reflect.macros.blackbox.Context
import language.experimental.macros

trait Impls {
    def impl(c: Context)(x: c.Expr[Any]) = x
}

object Macros extends Impls {
    def foo(x: Any) = macro impl
}