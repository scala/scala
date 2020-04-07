package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.SafeEq

import scala.tools.nsc.tasty.TastyUniverse
import scala.tools.nsc.tasty.Signature.MethodSignature
import scala.tools.nsc.tasty.TastyName
import scala.tools.nsc.tasty.TastyModes._
import scala.tools.nsc.tasty.Signature.NotAMethod

trait SymbolOps { self: TastyUniverse =>
  import self.{symbolTable => u}
  import FlagSets._

  @inline final def noSymbol: Symbol = u.NoSymbol
  @inline final def isSymbol(sym: Symbol): Boolean = sym ne u.NoSymbol

  implicit class SymbolDecorator(sym: Symbol) {
    def completer: TastyLazyType = {
      assert(sym.rawInfo.isInstanceOf[TastyLazyType], s"Expected TastyLazyType, is ${u.showRaw(sym.rawInfo: Type)} ")
      sym.rawInfo.asInstanceOf[TastyLazyType]
    }
    def ensureCompleted(): Unit = sym.info
    def ref(args: List[Type]): Type = u.appliedType(sym, args)
    def ref: Type = sym.ref(Nil)
    def singleRef: Type = mkSingleType(noPrefix, sym)
    def termRef: Type = sym.preciseRef(noPrefix)
    def preciseRef(pre: Type): Type = u.typeRef(pre, sym, Nil)
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

  /** if isConstructor, make sure it has one non-implicit parameter list */
  def normalizeIfConstructor(termParamss: List[List[Symbol]], isConstructor: Boolean): List[List[Symbol]] =
    if (isConstructor &&
      (termParamss.isEmpty || termParamss.head.nonEmpty && termParamss.head.head.is(Implicit)))
      Nil :: termParamss
    else
      termParamss

  def namedMemberOfType(space: Type, tname: TastyName, selectingTerm: Boolean)(implicit ctx: Context): Symbol = {
    val selector = encodeTastyName(if (selectingTerm) tname else tname.toTypeName)
    tname.signature match {
      case NotAMethod => memberOfSpace(space, selector, tname.isModuleName)
      case sig        => signedMemberOfSpace(space, selector, sig.map(resolveErasedTypeRef).asMethod)
    }
  }

  private def memberOfSpace(space: Type, name: u.Name, isModuleName: Boolean)(implicit ctx: Context): Symbol = {
    // TODO [tasty]: dotty uses accessibleDenot which asserts that `fetched.isAccessibleFrom(pre)`,
    //    or else filters for non private.
    // There should be an investigation to see what code makes that false, and what is an equivalent check.
    def lookInTypeCtor = space.typeConstructor.typeParams.filter(_.name == name).headOption.getOrElse(noSymbol)
    val fetched = space.member(name)
    val corrected = if (name.isTypeName) fetched.orElse(lookInTypeCtor) else fetched
    val finalSym = if (isModuleName) corrected.linkedClassOfClass else corrected
    if (isSymbol(finalSym)) {
      finalSym
    } else {
      val kind = if (name.isTermName) "term" else "type"
      def addendum(name: String) =
        if (ctx.mode.is(ReadParents)) s"$kind in parents of ${if (ctx.owner.isLocalDummy) ctx.owner.owner else ctx.owner}: $name"
        else if (ctx.owner.isClass) s"$kind required by a member of ${ctx.owner}: $name"
        else s"$kind $name while unpickling ${ctx.owner}"
      val msg =
        if (name.isTypeName && space.typeSymbol.hasPackageFlag)
          s"can't find ${addendum(s"${space.typeSymbol.fullNameString}.$name")}; perhaps it is missing from the classpath."
        else
          s"can't find ${addendum("" + name)}, in $space"
      typeError(msg)
    }
  }

  private def signedMemberOfSpace(space: Type, name: u.Name, sig: MethodSignature[Type])(implicit ctx: Context): Symbol = {
    ctx.log(s"""looking for overload member[$space]("$name") @@ ${sig.show}""")
    val member = space.member(name)
    val (tyParamCount, argTpes) = {
      val (tyParamCounts, params) = sig.params.partitionMap(identity)
      if (tyParamCounts.length > 1) {
        unsupportedError(s"multiple type parameter lists on erased method signature ${sig.show}")
      }
      (tyParamCounts.headOption.getOrElse(0), params)
    }
    def compareSym(sym: Symbol): Boolean = sym match {
      case sym: MethodSymbol =>
        val params = sym.paramss.flatten
        sym.returnType.erasure =:= sig.result &&
        params.length === argTpes.length &&
        (name === u.nme.CONSTRUCTOR && tyParamCount === member.owner.typeParams.length
          || tyParamCount === sym.typeParams.length) &&
        params.zip(argTpes).forall { case (param, tpe) => param.tpe.erasure =:= tpe } && {
          ctx.log(s"selected ${showSym(sym)} : ${sym.tpe}")
          true
        }
      case _ =>
        ctx.log(s"""member[$space]("$name") ${showSym(sym)} is not a method""")
        false
    }
    member.asTerm.alternatives.find(compareSym).getOrElse(
      typeError(s"No matching overload of $space.$name with signature ${sig.show}"))
  }

  def showSym(sym: Symbol): String = s"Symbol($sym, #${sym.hashCode})"
}
