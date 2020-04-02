package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.SafeEq

import scala.tools.nsc.tasty.TastyUniverse
import scala.tools.nsc.tasty.Signature
import scala.tools.nsc.tasty.Signature.MethodSignature
import scala.tools.nsc.tasty.TastyName
import scala.tools.nsc.tasty.Signature.NotAMethod

trait SymbolOps { self: TastyUniverse =>
  import Contexts.Context

  object SymbolOps {
    implicit class SymbolDecorator(sym: Symbol) {
      def completer: TastyLazyType = {
        assert(sym.rawInfo.isInstanceOf[TastyLazyType], s"Expected TastyLazyType, is ${showRaw(sym.rawInfo)} ")
        sym.rawInfo.asInstanceOf[TastyLazyType]
      }
      def ensureCompleted(): Unit = sym.info
      def ref(args: List[Type]): Type = mkAppliedType(sym, args)
      def ref: Type = sym.ref(Nil)
      def singleRef: Type = mkSingleType(noPrefix, sym)
      def termRef: Type = sym.preciseRef(noPrefix)
      def preciseRef(pre: Type): Type = mkTypeRef(pre, sym, Nil)
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

  def constructorOfType(space: Type): Either[String, Symbol] =
    space.member(nme.CONSTRUCTOR).asTerm.alternatives.find(_.isInstanceOf[MethodSymbol]).toRight(
      s"${space.typeSymbol} has no constructor"
    )

  def namedMemberOfType(space: Type, tname: TastyName, selectingTerm: Boolean)(implicit ctx: Context): Either[String, Symbol] = {
    val selector = encodeTastyName(tname, selectingTerm)
    tname.signature match {
      case NotAMethod => memberOfSpace(space, selector, tname.isModuleName)
      case sig        => signedMemberOfSpace(space, selector, sig.map(erasedNameToErasedType))
    }
  }

  private def memberOfSpace(space: Type, name: Name, isModuleName: Boolean): Either[String, Symbol] = {
    // TODO [tasty]: dotty uses accessibleDenot which asserts that `fetched.isAccessibleFrom(pre)`,
    //    or else filters for non private.
    // There should be an investigation to see what code makes that false, and what is an equivalent check.
    def lookInTypeCtor = space.typeConstructor.typeParams.filter(_.name == name).headOption.getOrElse(noSymbol)
    val fetched = space.member(name)
    val corrected = if (name.isTypeName) fetched.orElse(lookInTypeCtor) else fetched
    val finalSym = if (isModuleName) corrected.linkedClassOfClass else corrected
    if (isSymbol(finalSym)) {
      Right(finalSym)
    } else {
      val kind = if (name.isTermName) "term" else "type"
      val msg =
        if (space.typeSymbol.isPackage)
          s"can't find $kind $name in package ${space.typeSymbol.fullNameString}, perhaps it is missing from the classpath."
        else
          s"can't find $kind $name in $space"
      Left(msg)
    }
  }

  private def signedMemberOfSpace(space: Type, name: Name, sig: Signature[Type])(implicit ctx: Context): Either[String, Symbol] = {
    ctx.log(s"""looking for overload member[$space]("$name") @@ ${sig.show}""")
    val MethodSignature(args, ret) = sig
    val member = space.member(name)
    val (tyParamCount, argTpes) = {
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
        params.length === argTpes.length &&
        (name === nme.CONSTRUCTOR && tyParamCount === member.owner.typeParams.length
          || tyParamCount === sym.typeParams.length) &&
        params.zip(argTpes).forall { case (param, tpe) => param.tpe.erasure =:= tpe } && {
          ctx.log(s"selected ${showSym(sym)} : ${sym.tpe}")
          true
        }
      case _ =>
        ctx.log(s"""member[$space]("$name") ${showSym(sym)} is not a method""")
        false
    }
    member.asTerm.alternatives.find(compareSym).toRight(
      s"No matching overload of $space.$name with signature ${sig.show}")
  }

  def showSym(sym: Symbol): String = s"Symbol($sym, #${sym.hashCode})"
}
