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

import scala.annotation._
import scala.tools.nsc.tasty.{SafeEq, TastyUniverse, ForceKinds, TastyModes}, TastyModes._, ForceKinds._
import scala.tools.tasty.{TastyName, Signature, TastyFlags}, TastyName.SignedName, Signature.MethodSignature, TastyFlags._
import scala.tools.tasty.ErasedTypeRef

import scala.tools.nsc.tasty.TreeUnpickler.MaybeCycle.NoCycle

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

  private final def deepComplete(space: Type)(implicit ctx: Context): Unit = {
    symOfType(space) match {
      case u.NoSymbol =>
        ctx.log(s"could not retrieve symbol from type ${showType(space)}")
      case termSym if termSym.isTerm =>
        if (termSym.is(Object)) {
          termSym.ensureCompleted(SpaceForce)
          termSym.moduleClass.ensureCompleted(DeepForce | SpaceForce)
        }
        else {
          ctx.log(s"deep complete on non-module term ${showSym(termSym)}, not taking action")
        }
      case typeSym =>
        typeSym.ensureCompleted(SpaceForce)
    }
  }

  /** Fetch the symbol of a path type without forcing the symbol,
   * `NoSymbol` if not a path.
   */
  @tailrec
  private[bridge] final def symOfType(tpe: Type): Symbol = tpe match {
    case tpe: u.TypeRef => tpe.sym
    case tpe: u.SingleType => tpe.sym
    case tpe: u.ThisType => tpe.sym
    case tpe: u.ConstantType => symOfType(tpe.value.tpe)
    case tpe: u.ClassInfoType => tpe.typeSymbol
    case tpe: u.RefinedType0 => tpe.typeSymbol
    case tpe: u.ExistentialType => symOfType(tpe.underlying)
    case _ => u.NoSymbol
  }

  implicit final class SymbolDecorator(val sym: Symbol) {

    def isScala3Inline: Boolean = repr.tflags.is(Inline)
    def isScala2Macro: Boolean = repr.tflags.is(FlagSets.Scala2Macro)
    def isTraitParamAccessor: Boolean = sym.owner.isTrait && repr.tflags.is(FieldAccessor|ParamSetter)

    def isParamGetter: Boolean =
      sym.isMethod && sym.repr.tflags.is(FlagSets.ParamGetter)

    /** A computed property that should only be called on a symbol which is known to have been initialised by the
     *  Tasty Unpickler and is not yet completed.
     *
     *  @todo adapt callsites and type so that this property is more safe to call (barring mutation from uncontrolled code)
     */
    def repr: TastyRepr = {
      try sym.rawInfo.asInstanceOf[TastyRepr]
      catch {
        case _: ClassCastException =>
          val raw = u.showRaw(sym.rawInfo)
          val tastyRepr = u.typeOf[TastyRepr]
          throw new AssertionError(s"$sym is already completed. Expected $tastyRepr, is $raw.")
      }
    }

    def ensureCompleted(forceKinds: ForceKinds)(implicit ctx: Context): Unit = {
      val raw = sym.rawInfo
      if (raw.isInstanceOf[u.LazyType]) {
        ctx.trace(traceForceInfo(sym, forceKinds)) {
          sym.info
          sym.annotations.foreach(_.completeInfo())
        }
      } else {
        assert(!raw.isInstanceOf[TastyRepr], s"${showSym(sym)} has incorrectly initialised info $raw")
      }
    }

    private def traceForceInfo(
      sym: Symbol,
      forceKinds: ForceKinds
    )(implicit ctx: Context) = TraceInfo[Unit](
      query = "force symbol info",
      qual = s"${showSym(sym)} in context ${showSym(ctx.owner)}",
      res = _ => s"${showSym(sym)} was forced",
      modifiers = forceKinds.describe
    )

    def objectImplementation: Symbol = sym.moduleClass
    def sourceObject: Symbol = sym.sourceModule
    def ref: Type = u.appliedType(sym, Nil)
    def safeOwner: Symbol = if (sym.owner eq sym) sym else sym.owner
  }

  /** Is this symbol annotated with `scala.annotation.experimental`? */
  def symIsExperimental(sym: Symbol) = sym.hasAnnotation(defn.ExperimentalAnnotationClass)

  /** if isConstructor, make sure it has one non-implicit parameter list */
  def normalizeIfConstructor(@unused owner: Symbol, termParamss: List[List[Symbol]], paramClauses: List[List[NoCycle]], isConstructor: Boolean): List[List[Symbol]] =
    if (!isConstructor) termParamss
    else
      paramClauses match {
        case (vparam :: _) :: _ if vparam.tflags.is(Implicit, butNot=Given) => Nil :: termParamss
        case _ =>
          if (paramClauses.forall(paramClause => paramClause.nonEmpty && paramClause.head.tflags.is(Given))) {
            termParamss :+ Nil
          } else {
            termParamss
          }
      }

  private[bridge] def lookupSymbol(space: Type, tname: TastyName)(implicit ctx: Context): Symbol = {
    deepComplete(space)
    tname match {
      case SignedName(qual, sig, target) => lookupSigned(space, qual, sig.map(_.encode), target)
      case _                             => lookupSimple(space, tname)
    }
  }

  private def lookupSimple(space: Type, tname: TastyName)(implicit ctx: Context): Symbol = {
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
    else if (ctx.isJava && space.termSymbol.isModule && !space.termSymbol.hasPackageFlag) {
      // TODO [tasty]: remove this workaround for https://github.com/scala/scala3/issues/19619
      //   Use heuristic that we are accidentally looking in the static scope for some class/object,
      //   when really we should be looking in the instance scope. In this case, we should be always looking for
      //   the class and not the object, so we convert to type name and look in the companion.
      //
      //   we have the added bonus that we are looking for an inner class defined in the same TASTy file,
      //   so there should be no cross-file issues.
      val space0 = space.typeSymbol.companionClass.typeOfThis
      val tname0 = tname.toTypeName

      val secondTry = lookupSymbol(space0, tname0)
      if (secondTry.isClass) secondTry // avoid type parameters
      else errorMissing(space0, tname0)
    }
    else errorMissing(space, tname)
  }

  private def hasType(member: Symbol)(implicit ctx: Context) = {
    ctx.mode.is(ReadAnnotation) || ctx.mode.is(ReadMacro) && (member.info `ne` u.NoType) || (member.rawInfo `ne` u.NoType)
  }

  private def errorMissing[T](space: Type, tname: TastyName)(implicit ctx: Context) = {
    val kind = if (tname.isTypeName) "type" else "term"
    def typeToString(tpe: Type) = {
      def isPath(pre: Type) =
        pre.isInstanceOf[u.SingletonType] || pre.termSymbol.isModule || pre.typeSymbol.isModuleClass
      def inner(sb: StringBuilder, tpe: Type): StringBuilder = tpe match {
        case u.ThisType(cls) =>
          val isPackage = cls.hasPackageFlag
          sb append cls.fullNameString append (if (isPackage) "" else ".this")
        case u.SingleType(pre, sym) =>
          if ((pre eq u.NoPrefix) || (pre eq u.NoType)) sb append sym.nameString append ".type"
          else inner(sb, pre) append '.' append sym.nameString append ".type"
        case u.TypeRef(pre, sym, _) =>
          val sep = if (isPath(pre)) "." else "#"
          if ((pre eq u.NoPrefix) || (pre eq u.NoType)) sb append sym.nameString
          else inner(sb, pre) append sep append sym.nameString
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

  private def lookupSigned(
      space: Type,
      qual: TastyName,
      sig: MethodSignature[ErasedTypeRef],
      target: TastyName
  )(implicit ctx: Context): Symbol = {
    if (target ne qual) {
      unsupportedError(s"selection of method $qual with @targetName(" + '"' + target + '"' + ")")
    }
    else {
      ctx.trace(traceOverload(space, qual, sig)) {
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
            sym.ensureCompleted(OverloadedSym)
            // TODO [tasty]: we should cache signatures for symbols and compare against `sig`
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
        )
      }
    }
  }

  private def traceOverload(space: Type, tname: TastyName, sig: MethodSignature[ErasedTypeRef]) = TraceInfo[Symbol](
    query = s"looking for overload",
    qual = s"symbolOf[$space] @@ $tname: ${showSig(sig)}",
    res = overload => s"selected overload ${showSym(overload)}"
  )

  def showSig(sig: MethodSignature[ErasedTypeRef]): String = sig.map(_.signature).show
  def showSym(sym: Symbol): String = s"`(#${sym.id}) ${sym.accurateKindString} ${sym.name}`"
  def showSymStable(sym: Symbol): String = s"#[${sym.id}, ${sym.name}]"
}
