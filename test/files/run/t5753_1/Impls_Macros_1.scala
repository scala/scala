import scala.reflect.macros.BlackboxContext
import language.experimental.macros

trait Impls {
    def impl(c: BlackboxContext)(x: c.Expr[Any]) = x
}

object Macros extends Impls {
    def foo(x: Any) = macro impl
}