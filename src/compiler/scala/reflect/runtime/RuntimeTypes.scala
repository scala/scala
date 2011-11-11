package scala.reflect
package runtime

import collection.mutable.ListBuffer

trait RuntimeTypes extends Universe with api.RuntimeTypes {

  /** To lift path dependent types into reflection, we use InstanceRefSymbols.
   *  Two of these are equal if they point to the same object reference. Todo: remove
   */
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