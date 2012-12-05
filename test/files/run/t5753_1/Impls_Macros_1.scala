import scala.reflect.macros.Context
import language.experimental.macros

trait Impls {
    def impl(c: Context)(x: c.Expr[Any]) = x
}

object Macros extends Impls {
    def foo(x: Any) = macro impl
}