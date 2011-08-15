package scala.reflect
package runtime

import collection.mutable.ListBuffer

trait RuntimeTypes extends Universe with api.RuntimeTypes {

  def freeValue(x: Any): Tree = FreeValue(x)

  // to do: replace with generalized
  // case class Literal(x: Any),
  // once calls to the deprecated factory Literal(x: Any) has been eliminated from all code.
  case class FreeValue(any: Any) extends Tree

  case class InstanceRefSymbol(value: AnyRef) extends TermSymbol(NoSymbol, NoPosition, nme.EMPTY)
  object InstanceRefSymbol extends InstanceRefSymbolExtractor

  override private[reflect] def namedType(pre: Type, sym: Symbol, args: List[Type]): Type = {
    val tparamBuf = new ListBuffer[Symbol]
    val args1 = for (arg <- args) yield arg match {
      case _: TypeBounds =>
        val ex = pre.typeSymbol.freshExistential("$ex") setInfo arg
        tparamBuf += ex
        TypeRef(NoPrefix, ex, List())
      case _ =>
        arg
    }
    existentialAbstraction(tparamBuf.toList, typeRef(pre, sym, args1))
  }

}