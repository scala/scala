import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[U <: Int](c: Ctx) = ???
}
