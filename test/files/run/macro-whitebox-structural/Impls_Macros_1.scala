import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
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