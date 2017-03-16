package scala.reflect
package internal
package transform

trait PostErasure {
  val global: SymbolTable
  import global._

  object elimErasedValueType extends TypeMap {
    def apply(tp: Type) = tp match {
      case ConstantType(Constant(tp1: Type)) =>
        val tp1Mapped = apply(tp1)
        if (tp1 eq tp1Mapped) tp
        else ConstantType(Constant(tp1Mapped))
      case ErasedValueType(_, underlying)   => underlying
      case _                                => mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type) = elimErasedValueType(tp)
}
