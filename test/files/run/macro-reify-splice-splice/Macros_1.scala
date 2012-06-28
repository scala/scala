import scala.reflect.makro.{Context => Ctx}

object Macros {
  def foo = macro Impls.foo

  object Impls {
    def foo(c: Ctx) = c.reify {
      { c.reify(c.reify("hello world")) }.splice.splice
    }
  }
}