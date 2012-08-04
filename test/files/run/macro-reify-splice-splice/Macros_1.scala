import scala.reflect.macros.{Context => Ctx}

object Macros {
  def foo = macro Impls.foo

  object Impls {
    def foo(c: Ctx) = c.universe.reify {
      { c.universe.reify(c.universe.reify("hello world")) }.splice.splice
    }
  }
}