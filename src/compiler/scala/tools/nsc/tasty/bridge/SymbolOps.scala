/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.SafeEq

import scala.tools.nsc.tasty.{TastyUniverse, TastyModes}, TastyModes._
import scala.tools.tasty.{TastyName, Signature, TastyFlags}, TastyName.SignedName, Signature.MethodSignature, TastyFlags.TastyFlagSet

trait SymbolOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  @inline final def noSymbol: Symbol = u.NoSymbol
  @inline final def isSymbol(sym: Symbol): Boolean = sym ne u.NoSymbol

  def allowsOverload(sym: Symbol) = ( // TODO [tasty]: taken from Namer. Added module symbols
    (sym.isSourceMethod || sym.isModule) && sym.owner.isClass && !sym.isTopLevel
  )

  implicit class SymbolDecorator(val sym: Symbol) {
    def completer: TastyLazyType = {
      assert(sym.rawInfo.isInstanceOf[TastyLazyType], s"Expected TastyLazyType, is ${u.showRaw(sym.rawInfo: Type)} ")
      sym.rawInfo.asInstanceOf[TastyLazyType]
    }
    def ensureCompleted(): Unit = {
      sym.info
      sym.annotations.foreach(_.completeInfo())
    }
    def ref(args: List[Type]): Type = u.appliedType(sym, args)
    def ref: Type = sym.ref(Nil)
    def singleRef: Type = u.singleType(u.NoPrefix, sym)
    def termRef: Type = sym.preciseRef(u.NoPrefix)
    def preciseRef(pre: Type): Type = u.typeRef(pre, sym, Nil)
    def safeOwner: Symbol = if (sym.owner eq sym) sym else sym.owner

    def set(mask: TastyFlagSet)(implicit ctx: Context): sym.type = ctx.addFlags(sym, mask)
    def reset(mask: TastyFlagSet)(implicit ctx: Context): sym.type = ctx.removeFlags(sym, mask)

    def isOneOf(mask: TastyFlagSet): Boolean = sym.hasFlag(encodeFlagSet(mask))
    def is(mask: TastyFlagSet): Boolean = sym.hasAllFlags(encodeFlagSet(mask))
    def is(mask: TastyFlagSet, butNot: TastyFlagSet): Boolean =
      if (!butNot)
        sym.is(mask)
      else
        sym.is(mask) && sym.not(butNot)
    def not(mask: TastyFlagSet): Boolean = sym.hasNoFlags(encodeFlagSet(mask))
  }

  /** if isConstructor, make sure it has one non-implicit parameter list */
  def normalizeIfConstructor(termParamss: List[List[Symbol]], isConstructor: Boolean): List[List[Symbol]] =
    if (isConstructor &&
      (termParamss.isEmpty || termParamss.head.nonEmpty && termParamss.head.head.isImplicit))
      Nil :: termParamss
    else
      termParamss

  def namedMemberOfType(space: Type, tname: TastyName)(implicit ctx: Context): Symbol = tname match {
    case SignedName(qual, sig) => signedMemberOfSpace(space, qual, sig.map(resolveErasedTypeRef))
    case _                     => memberOfSpace(space, tname)
  }

  private def memberOfSpace(space: Type, tname: TastyName)(implicit ctx: Context): Symbol = {
    // TODO [tasty]: dotty uses accessibleDenot which asserts that `fetched.isAccessibleFrom(pre)`,
    //    or else filters for non private.
    // There should be an investigation to see what code makes that false, and what is an equivalent check.
    val member = {
      if (tname.isTypeName) {
        val asTerm = tname.toTermName
        if (asTerm.isModuleName) space.member(encodeTermName(asTerm)).moduleClass
        else {
          val selector = encodeTastyName(tname)
          def lookInTypeCtor =
            space.typeConstructor.typeParams.filter(selector === _.name).headOption.getOrElse(noSymbol)
          space.member(selector).orElse(lookInTypeCtor)
        }
      }
      else space.member(encodeTermName(tname))
    }
    if (isSymbol(member)) member
    else {
      val kind = if (tname.isTypeName) "type" else "term"
      def addendum(name: String) =
        if (ctx.mode.is(ReadParents)) s"$kind in parents of ${if (ctx.owner.isLocalDummy) ctx.owner.owner else ctx.owner}: $name"
        else if (ctx.owner.isClass) s"$kind required by a member of ${ctx.owner}: $name"
        else s"$kind $name while unpickling ${ctx.owner}"
      val msg =
        if (tname.isTypeName && space.typeSymbol.hasPackageFlag)
          s"can't find ${addendum(s"${space.typeSymbol.fullNameString}.$tname")}; perhaps it is missing from the classpath."
        else
          s"can't find ${addendum("" + tname)}, in $space"
      typeError(msg)
    }
  }

  private def signedMemberOfSpace(space: Type, qual: TastyName, sig: MethodSignature[Type])(implicit ctx: Context): Symbol = {
    ctx.log(s"""looking for overload member[$space]("$qual") @@ ${sig.show}""")
    val member = space.member(encodeTermName(qual))
    val (tyParamCount, argTpes) = {
      val (tyParamCounts, params) = sig.params.partitionMap(identity)
      if (tyParamCounts.length > 1) {
        unsupportedError(s"multiple type parameter lists on erased method signature ${sig.show}")
      }
      (tyParamCounts.headOption.getOrElse(0), params)
    }
    def compareSym(sym: Symbol): Boolean = sym match {
      case sym: u.MethodSymbol =>
        val params = sym.paramss.flatten
        sym.returnType.erasure =:= sig.result &&
        params.length === argTpes.length &&
        (qual === TastyName.Constructor && tyParamCount === member.owner.typeParams.length
          || tyParamCount === sym.typeParams.length) &&
        params.zip(argTpes).forall { case (param, tpe) => param.tpe.erasure =:= tpe } && {
          ctx.log(s"selected ${showSym(sym)} : ${sym.tpe}")
          true
        }
      case _ =>
        ctx.log(s"""member[$space]("$qual") ${showSym(sym)} is not a method""")
        false
    }
    member.asTerm.alternatives.find(compareSym).getOrElse(
      typeError(s"No matching overload of $space.$qual with signature ${sig.show}"))
  }

  def showSym(sym: Symbol): String = s"Symbol($sym, #${sym.hashCode})"
}
