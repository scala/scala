import scala.reflect.makro.Context

object Impls {
  def impl[A](c: reflect.makro.Context) = c.reify(())
}

object Macros {
  def decl[A] = macro Impls.impl[A]
}