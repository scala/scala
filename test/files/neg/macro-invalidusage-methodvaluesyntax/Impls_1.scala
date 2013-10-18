import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.universe._
    c.Expr[Unit](q"""println("it works")""")
  }
}