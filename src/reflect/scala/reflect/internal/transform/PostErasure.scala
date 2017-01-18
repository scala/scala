package scala.reflect
package internal
package transform

trait PostErasure {

  val global: SymbolTable
  import global._
  import definitions._

  object elimErasedValueType extends TypeMap {
    def apply(tp: Type) = tp match {
      case ConstantType(Constant(tp: Type)) =>
        ConstantType(Constant(apply(tp)))
      case ErasedValueType(tref) =>
        atPhase(erasurePhase)(erasure.erasedValueClassArg(tref))
      case _ => mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type) = elimErasedValueType(tp)
}
