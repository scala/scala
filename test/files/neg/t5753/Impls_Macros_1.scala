import scala.reflect.macros.{BlackboxContext => Ctx}

trait Impls {
def impl(c: Ctx)(x: c.Expr[Any]) = x
}

