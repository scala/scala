import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(unconventionalName: Ctx)(x: unconventionalName.Expr[Int]) = {
    import unconventionalName.universe._
    unconventionalName.Expr[Unit](q"""println("invoking foo...")""")
  }
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
