package test56

import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context
import scala.reflect.api.Universe

object Impls {
  def materializeTypeTag_impl[T: c.WeakTypeTag](c: Context)(u: c.Expr[Universe])(e: c.Expr[T]): c.Expr[u.value.TypeTag[T]] = ???
}