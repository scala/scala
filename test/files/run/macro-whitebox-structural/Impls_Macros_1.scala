import scala.reflect.macros.WhiteboxContext
import scala.language.experimental.macros

object Macros {
  def impl(c: WhiteboxContext) = {
    import c.universe._
    q"""
      trait Foo {
        def x = 2
      }
      new Foo {}
    """
  }

  def foo: Any = macro impl
}