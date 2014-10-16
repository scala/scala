/* NSC -- new Scala compiler
 * Copyright 2014-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal
package transform

trait PostErasure {
  val global: SymbolTable
  import global._
  import definitions._

  object elimErasedValueType extends TypeMap {
    def apply(tp: Type) = tp match {
      case ConstantType(Constant(tp: Type)) => ConstantType(Constant(apply(tp)))
      case ErasedValueType(_, underlying)   => underlying
      case _                                => mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type) = elimErasedValueType(tp)
}
