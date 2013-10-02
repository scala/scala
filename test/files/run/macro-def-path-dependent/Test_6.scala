package test56

import scala.reflect.runtime.universe._
import scala.reflect.macros.BlackboxContext
import scala.reflect.api.Universe

object Macros {
  def materializeTypeTag[T](u: Universe)(e: T) = macro Impls.materializeTypeTag_impl[T]
}