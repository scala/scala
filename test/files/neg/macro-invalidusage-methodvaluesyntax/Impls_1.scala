import scala.reflect.macros.{BlackboxContext => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.universe._
    c.Expr[Unit](q"""println("it works")""")
  }
}