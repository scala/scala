import scala.reflect.macros.blackbox.Context

object Macros {
  def foo = macro Impls.foo

  object Impls {
    def foo(c: Context) = c.universe.reify {
      { c.universe.reify(c.universe.reify("hello world")) }.splice.splice
    }
  }
}