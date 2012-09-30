import scala.reflect.macros.{Context => Ctx}

object Macros {
  def foo(s: String) = macro Impls.foo

  object Impls {
    def foo(c: Ctx)(s: c.Expr[String]) = c.universe.reify {
      println("hello " + s.splice)
    }
  }
}