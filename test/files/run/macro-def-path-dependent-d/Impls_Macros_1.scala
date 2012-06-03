import scala.reflect.runtime.universe._
import scala.reflect.makro.Context
import scala.reflect.api.Universe

object Test {
  def materializeTypeTag[T](u: Universe)(e: T) = macro materializeTypeTag_impl[T]

  def materializeTypeTag_impl[T: c.TypeTag](c: Context)(u: c.Expr[Universe])(e: c.Expr[T]): c.Expr[u.value.TypeTag[T]] = ???
}