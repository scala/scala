// See https://github.com/typelevel/scala/issues/139

import scala.collection.mutable

object Test {
  // Compiles, as expected
  def pass(a: AnyRef): mutable.Buffer[_ <: a.type] = {
    mutable.Buffer.empty[a.type]
  }

  // Expect compile, got error
  def error(a: Any): mutable.Buffer[_ <: a.type] = {
    mutable.Buffer.empty[a.type]
  }

  class Foo[T]
  def error1(a: Any): Foo[_ <: a.type] = new Foo[a.type]
}
