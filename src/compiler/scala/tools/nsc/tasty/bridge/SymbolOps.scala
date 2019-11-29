package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyUniverse

trait SymbolOps extends TastyKernel { self: TastyUniverse =>

  object SymbolOps {
    implicit class SymbolDecorator(sym: Symbol) {
      def completer: TastyLazyType = {
        assert(sym.rawInfo.isInstanceOf[TastyLazyType], s"Expected TastyLazyType, is ${showRaw(sym.rawInfo)} ")
        sym.rawInfo.asInstanceOf[TastyLazyType]
      }
      def ensureCompleted(): Unit = sym.info
      def typeRef(args: List[Type]): Type = symbolTable.typeRef(sym.owner.toType, sym, args)
      def typeRef: Type = symbolTable.typeRef(sym.owner.toType, sym, Nil)
      def termRef: Type = symbolTable.typeRef(sym.owner.toType, sym, Nil)
      def safeOwner: Symbol = if (sym.owner eq sym) sym else sym.owner
      def isOneOf(mask: FlagSet): Boolean = sym.hasFlag(mask)
      def is(mask: FlagSet): Boolean = sym.hasAllFlags(mask)
      def is(mask: FlagSet, butNot: FlagSet): Boolean =
        if (isEmpty(butNot))
          sym.hasFlag(mask)
        else
          sym.hasFlag(mask) && sym.hasNoFlags(butNot)
      def not(mask: FlagSet): Boolean = !is(mask)
    }
  }

  def showSym(sym: Symbol): String = s"$sym # ${sym.hashCode}"
}
