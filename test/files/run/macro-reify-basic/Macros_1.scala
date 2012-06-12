import scala.reflect.makro.{Context => Ctx}

object Macros {
  def foo(s: String) = macro Impls.foo

  object Impls {
    def foo(c: Ctx)(s: c.Expr[String]) = c.reify {
      println("hello " + s.splice)
    }
  }
}