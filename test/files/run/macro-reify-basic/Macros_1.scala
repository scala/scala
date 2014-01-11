import scala.reflect.macros.blackbox.Context

object Macros {
  def foo(s: String) = macro Impls.foo

  object Impls {
    def foo(c: Context)(s: c.Expr[String]) = c.universe.reify {
      println("hello " + s.splice)
    }
  }
}