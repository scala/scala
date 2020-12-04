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
import scala.tools.tasty.{TastyName, Signature, TastyFlags}, TastyName.SignedName, Signature.MethodSignature, TastyFlags._
import scala.tools.tasty.ErasedTypeRef

/**This layer deals with selecting a member symbol from a type using a `TastyName`,
 * also contains factories for making type references to symbols.
 */
trait SymbolOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  @inline final def noSymbol: Symbol = u.NoSymbol
  @inline final def isSymbol(sym: Symbol): Boolean = sym ne u.NoSymbol

  final def allowsOverload(sym: Symbol) = ( // TODO [tasty]: taken from Namer. Added module symbols
    (sym.isSourceMethod || sym.isModule) && sym.owner.isClass && !sym.isTopLevel
  )

  final def declaringSymbolOf(sym: Symbol): Symbol =
    if (sym.isModuleClass) sym.sourceModule else sym

  implicit final class SymbolDecorator(val sym: Symbol) {

    def isScala3Macro: Boolean = repr.originalFlagSet.is(Inline | Macro)
    def isScala3Inline: Boolean = repr.originalFlagSet.is(Inline)
    def isScala2Macro: Boolean = repr.originalFlagSet.is(Erased | Macro)

    def isPureMixinCtor: Boolean = isMixinCtor && repr.originalFlagSet.is(Stable)
    def isMixinCtor: Boolean = u.nme.MIXIN_CONSTRUCTOR == sym.name && sym.owner.isTrait

    def isTraitParamAccessor: Boolean = sym.owner.isTrait && repr.originalFlagSet.is(FieldAccessor|ParamSetter)

    def isParamGetter: Boolean =
      sym.isMethod && sym.repr.originalFlagSet.is(FlagSets.FieldAccessorFlags)

    /** A computed property that should only be called on a symbol which is known to have been initialised by the
     *  Tasty Unpickler and is not yet completed.
     *
     *  @todo adapt callsites and type so that this property is more safe to call (barring mutation from uncontrolled code)
     */
    def repr: TastyRepr = {
      require(sym.rawInfo.isInstanceOf[TastyRepr], s"Expected ${u.typeOf[TastyRepr]}, is ${u.showRaw(sym.rawInfo)} ")
      sym.rawInfo.asInstanceOf[TastyRepr]
    }

    def ensureCompleted(): Unit = {
      sym.info
      sym.annotations.foreach(_.completeInfo())
    }
    def objectImplementation: Symbol = sym.moduleClass
    def sourceObject: Symbol = sym.sourceModule
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
    case SignedName(qual, sig, target) => signedMemberOfSpace(space, qual, sig.map(_.encode), target)
    case _                             => memberOfSpace(space, tname)
  }

  private def memberOfSpace(space: Type, tname: TastyName)(implicit ctx: Context): Symbol = {
    // TODO [tasty]: dotty uses accessibleDenot which asserts that `fetched.isAccessibleFrom(pre)`,
    //    or else filters for non private.
    // There should be an investigation to see what code makes that false, and what is an equivalent check.
    val member = {
      if (tname.isTypeName) {
        val asTerm = tname.toTermName
        if (asTerm.isObjectName) space.member(encodeTermName(asTerm)).moduleClass
        else {
          val selector = encodeTastyName(tname)
          def lookInTypeCtor =
            space.typeConstructor.typeParams.filter(selector === _.name).headOption.getOrElse(noSymbol)
          space.member(selector).orElse(lookInTypeCtor)
        }
      }
      else space.member(encodeTermName(tname))
    }
    if (isSymbol(member) && hasType(member)) member
    else errorMissing(space, tname)
  }

  private def hasType(member: Symbol)(implicit ctx: Context) = {
    ctx.mode.is(ReadAnnotation) || ctx.mode.is(ReadMacro) && (member.info `ne` u.NoType) || (member.rawInfo `ne` u.NoType)
  }

  private def errorMissing[T](space: Type, tname: TastyName)(implicit ctx: Context) = {
    val kind = if (tname.isTypeName) "type" else "term"
    def typeToString(tpe: Type) = {
      def inner(sb: StringBuilder, tpe: Type): StringBuilder = tpe match {
        case u.SingleType(pre, sym) => inner(sb, pre) append '.' append (
          if (sym.isPackageObjectOrClass) s"`${sym.name}`"
          else String valueOf sym.name
        )
        case u.TypeRef(pre, sym, _) if sym.isTerm =>
          if ((pre eq u.NoPrefix) || (pre eq u.NoType)) sb append sym.name
          else inner(sb, pre) append '.' append sym.name
        case tpe => sb append tpe
      }
      inner(new StringBuilder(), tpe).toString
    }
    def addendum(name: String) = {
      if (ctx.mode.is(ReadParents)) s"$kind in parents of ${location(if (ctx.owner.isLocalDummy) ctx.owner.owner else ctx.owner)}: $name"
      else s"$kind required by ${location(ctx.owner)}: $name"
    }
    val missing = addendum(s"${typeToString(space)}.$tname")
    typeError(s"can't find $missing; perhaps it is missing from the classpath.")
  }

  private def signedMemberOfSpace(space: Type, qual: TastyName, sig: MethodSignature[ErasedTypeRef], target: TastyName)(implicit ctx: Context): Symbol = {
    if (target ne qual) {
      unsupportedError(s"selection of method $qual with @targetName(" + '"' + target + '"' + ")")
    }
    else {
      ctx.log(s"""<<< looking for overload in symbolOf[$space] @@ $qual: ${showSig(sig)}""")
      val member = space.member(encodeTermName(qual))
      if (!(isSymbol(member) && hasType(member))) errorMissing(space, qual)
      val (tyParamCount, argTpeRefs) = {
        val (tyParamCounts, params) = sig.params.partitionMap(identity)
        if (tyParamCounts.length > 1) {
          unsupportedError(s"multiple type parameter lists on erased method signature ${showSig(sig)}")
        }
        (tyParamCounts.headOption.getOrElse(0), params)
      }
      def compareSym(sym: Symbol): Boolean = sym match {
        case sym: u.MethodSymbol =>
          val method = sym.tpe.asSeenFrom(space, sym.owner)
          ctx.log(s">>> trying $sym: $method")
          val params = method.paramss.flatten
          val isJava = sym.isJavaDefined
          NameErasure.sigName(method.finalResultType, isJava) === sig.result &&
          params.length === argTpeRefs.length &&
          (qual === TastyName.Constructor && tyParamCount === member.owner.typeParams.length
            || tyParamCount === sym.typeParams.length) &&
          params.zip(argTpeRefs).forall { case (param, tpe) => NameErasure.sigName(param.tpe, isJava) === tpe } && {
            ctx.log(s">>> selected ${showSym(sym)}: ${sym.tpe}")
            true
          }
        case _ =>
          ctx.log(s"""! member[$space]("$qual") ${showSym(sym)} is not a method""")
          false
      }
      member.asTerm.alternatives.find(compareSym).getOrElse(
        typeError(s"No matching overload of $space.$qual with signature ${showSig(sig)}"))
    }
  }

  def showSig(sig: MethodSignature[ErasedTypeRef]): String = sig.map(_.signature).show
  def showSym(sym: Symbol): String = s"Symbol($sym, #${sym.id})"
}
