import scala.reflect.makro.{Context => Ctx}

trait MacroHelpers {
  object Impls {
    def foo(c: Ctx)(x: c.Expr[Any]) = x
  }
}