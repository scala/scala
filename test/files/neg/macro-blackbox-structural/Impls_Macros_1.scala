import scala.language.experimental.macros

object Macros {
  def impl(c: scala.reflect.macros.blackbox.Context) = {
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