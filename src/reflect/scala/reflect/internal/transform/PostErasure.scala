package scala.reflect
package internal
package transform

trait PostErasure {
  val global: SymbolTable
  import global._

  object elimErasedValueType extends TypeMap {
    def apply(tp: Type) = tp match {
      case ConstantType(Constant(tp: Type)) => ConstantType(Constant(apply(tp)))
      case ErasedValueType(_, underlying)   => underlying
      case null                             => null
      case _                                => tp.mapOver(this)
    }
  }

  def transformInfo(sym: Symbol, tp: Type) = elimErasedValueType(tp)
}
