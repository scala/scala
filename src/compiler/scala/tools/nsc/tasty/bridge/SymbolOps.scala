package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.SafeEq

import scala.tools.nsc.tasty.TastyUniverse
import scala.tools.nsc.tasty.Signature
import scala.tools.nsc.tasty.Signature.MethodSignature

trait SymbolOps extends TastyKernel { self: TastyUniverse =>
  import Contexts.Context

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

    implicit class FlattenOps(syms: List[Symbol]) {
      def filterSyms: List[Symbol] = syms.filter(isSymbol)
    }
  }

  def selectSymFromSig(qualType: Type, name: Name, sig: Signature[Type])(implicit ctx: Context): Option[(Int, Symbol)] = {
    ctx.log(s"""looking for overload member[$qualType]("$name") @@ ${sig.show}""")
    val MethodSignature(args, ret) = sig
    var seenTypeParams = false
    val member = qualType.member(name)
    val (tyParamCount, argsSyms) = {
      val (tyParamCounts, params) = args.partitionMap(identity)
      if (tyParamCounts.length > 1) {
        reporter.error(noPosition, s"Multiple type parameter lists on signature ${sig.show} for $member.")
      }
      (tyParamCounts.headOption.getOrElse(0), params)
    }
    def compareSym(sym: Symbol): Boolean = sym match {
      case sym: MethodSymbol =>
        val params = sym.paramss.flatten
        sym.returnType.erasure =:= ret &&
        params.length === argsSyms.length &&
        ((name === nme.CONSTRUCTOR && tyParamCount === member.owner.typeParams.length)
          || tyParamCount === sym.typeParams.length) &&
        params.zip(argsSyms).forall { case (param, tpe) => param.tpe.erasure =:= tpe }
      case _ =>
        ctx.log(s"""member[$qualType]("$name") ${showSym(sym)} is not a method""")
        false
    }
    member.asTerm.alternatives.find(compareSym).map(tyParamCount -> _)
  }

  def showSym(sym: Symbol): String = s"Symbol($sym, #${sym.hashCode})"
}
