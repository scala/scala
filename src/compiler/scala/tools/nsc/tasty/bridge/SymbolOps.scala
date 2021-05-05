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
import scala.util.chaining._

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

  private final def deepComplete(tpe: Type): Unit = {
    val asTerm = tpe.termSymbol
    if (asTerm ne u.NoSymbol) {
      asTerm.ensureCompleted()
      deepComplete(tpe.widen)
    } else {
      tpe.typeSymbol.ensureCompleted()
    }
  }

  implicit final class SymbolDecorator(val sym: Symbol) {

    def isScala3Inline: Boolean = repr.originalFlagSet.is(Inline)
    def isScala2Macro: Boolean = repr.originalFlagSet.is(FlagSets.Scala2Macro)
    def isTraitParamAccessor: Boolean = sym.owner.isTrait && repr.originalFlagSet.is(FieldAccessor|ParamSetter)

    def isParamGetter: Boolean =
      sym.isMethod && sym.repr.originalFlagSet.is(FlagSets.ParamGetter)

    /** A computed property that should only be called on a symbol which is known to have been initialised by the
     *  Tasty Unpickler and is not yet completed.
     *
     *  @todo adapt callsites and type so that this property is more safe to call (barring mutation from uncontrolled code)
     */
    def repr: TastyRepr = {
      try sym.rawInfo.asInstanceOf[TastyRepr]
      catch {
        case err: ClassCastException =>
          val raw = u.showRaw(sym.rawInfo)
          val tastyRepr = u.typeOf[TastyRepr]
          throw new AssertionError(s"$sym is already completed. Expected $tastyRepr, is $raw.")
      }
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
  }

  /** if isConstructor, make sure it has one non-implicit parameter list */
  def normalizeIfConstructor(termParamss: List[List[Symbol]], isConstructor: Boolean): List[List[Symbol]] =
    if (isConstructor &&
      (termParamss.isEmpty || termParamss.head.nonEmpty && termParamss.head.head.isImplicit))
      Nil :: termParamss
    else
      termParamss

  def namedMemberOfType(space: Type, tname: TastyName)(implicit ctx: Context): Symbol = {
    deepComplete(space)
    tname match {
      case SignedName(qual, sig, target) => signedMemberOfSpace(space, qual, sig.map(_.encode), target)
      case _                             => memberOfSpace(space, tname)
    }
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
      else {
        val firstTry = space.member(encodeTermName(tname))
        if (firstTry.isOverloaded) firstTry.filter(!_.isPrivateLocal)
        else firstTry
      }
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
        case u.ThisType(cls) => sb append cls.fullNameString
        case u.SingleType(pre, sym) =>
          if ((pre eq u.NoPrefix) || (pre eq u.NoType)) sb append sym.nameString
          else inner(sb, pre) append '.' append sym.nameString
        case u.TypeRef(pre, sym, _) =>
          if ((pre eq u.NoPrefix) || (pre eq u.NoType)) sb append sym.nameString
          else inner(sb, pre) append '.' append sym.nameString
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
      val (tyParamCount, paramRefs) = {
        val (tyParamCounts, params) = sig.params.partitionMap(identity)
        if (tyParamCounts.length > 1) {
          unsupportedError(s"method with unmergeable type parameters: $qual")
        }
        (tyParamCounts.headOption.getOrElse(0), params)
      }
      def compareSym(sym: Symbol): Boolean = sym match {
        case sym: u.MethodSymbol =>
          val meth0 = u.unwrapWrapperTypes(sym.tpe.asSeenFrom(space, sym.owner))
          val paramSyms = meth0.paramss.flatten
          val resTpe = meth0.finalResultType
          val sameParamSize = paramSyms.length === paramRefs.length
          def sameTyParamSize = tyParamCount === ({
            // the signature of a class/mixin constructor includes
            // type parameters, in nsc these come from the parent.
            val tyParamOwner = if (qual.isConstructorName) member.owner else sym
            tyParamOwner.typeParams.length
          })
          def sameParams = paramSyms.lazyZip(paramRefs).forall({
            case (paramSym, paramRef) => sameErasure(sym)(paramSym.tpe, paramRef)
          })
          sameParamSize && sameTyParamSize && sameParams && sameErasure(sym)(resTpe, sig.result)
        case _ =>
          ctx.log(s"""! member[$space]("$qual") ${showSym(sym)} is not a method""")
          false
      }
      member.asTerm.alternatives.find(compareSym).getOrElse(
        typeError(s"No matching overload of $space.$qual with signature ${showSig(sig)}")
      ).tap(overload =>
        ctx.log(s">>> selected ${showSym(overload)}: ${overload.tpe}")
      )
    }
  }

  def showSig(sig: MethodSignature[ErasedTypeRef]): String = sig.map(_.signature).show
  def showSym(sym: Symbol): String = s"Symbol(${sym.accurateKindString} ${sym.name}, #${sym.id})"
}
