package test56

import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context
import scala.reflect.api.Universe

object Macros {
  def materializeTypeTag[T](u: Universe)(e: T): u.TypeTag[T] = macro Impls.materializeTypeTag_impl[T]
}