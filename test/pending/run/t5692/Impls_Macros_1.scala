import scala.reflect.macros.Context

object Impls {
  def impl[A](c: reflect.macros.Context) = c.universe.reify(())
}

object Macros {
  def decl[A] = macro Impls.impl[A]
}