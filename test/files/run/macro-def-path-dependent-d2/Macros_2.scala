import scala.reflect.runtime.universe._
import scala.reflect.macros.Context
import scala.reflect.api.Universe

object Macros {
  def materializeTypeTag[T](u: Universe)(e: T) = macro Impls.materializeTypeTag_impl[T]
}