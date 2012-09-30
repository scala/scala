import scala.reflect.macros.{Context => Ctx}

trait ImplContainer {
  object Impls {
    def foo(c: Ctx)(x: c.Expr[Any]) = x
  }
}