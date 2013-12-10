import scala.language.experimental.macros

object Macros {
  def impl(c: scala.reflect.macros.BlackboxContext) = {
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