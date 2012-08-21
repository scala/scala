import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[U <: String](c: Ctx) = ???
}
