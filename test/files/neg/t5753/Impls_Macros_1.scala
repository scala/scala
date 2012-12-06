import scala.reflect.macros.{Context => Ctx}

trait Impls {
def impl(c: Ctx)(x: c.Expr[Any]) = x
}

