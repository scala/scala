package scala.reflect
package runtime

import collection.mutable.ListBuffer

trait RuntimeTypes extends Universe with api.RuntimeTypes {

  case class FreeVar(_name: TermName, _tpe: Type, value: Any) extends TermSymbol(definitions.RootClass, NoPosition, _name) {
    setInfo(_tpe)

    override def hashCode = value.hashCode

    override def equals(other: Any): Boolean = other match {
      case FreeVar(_, _, value1) => value.asInstanceOf[AnyRef] eq value1.asInstanceOf[AnyRef]
      case _ => false
    }
  }

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