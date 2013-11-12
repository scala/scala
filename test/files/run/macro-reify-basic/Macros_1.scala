import scala.reflect.macros.{BlackboxContext => Ctx}

object Macros {
  def foo(s: String) = macro Impls.foo

  object Impls {
    def foo(c: Ctx)(s: c.Expr[String]) = c.universe.reify {
      println("hello " + s.splice)
    }
  }
}