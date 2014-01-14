import scala.reflect.macros.blackbox.Context

object Impls {
  def foo[U <: String](c: Context) = ???
}
