import scala.reflect.macros.{BlackboxContext => Ctx}

object Impls {
  def foo[U <: String](c: Ctx): c.Expr[Unit] = { import c.universe._; c.Expr[Unit](q"""println("hello world")""") }
}
