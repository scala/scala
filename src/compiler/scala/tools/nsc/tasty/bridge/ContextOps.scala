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

import scala.annotation.tailrec
import scala.reflect.io.AbstractFile

import scala.tools.tasty.{TastyName, TastyFlags}, TastyFlags._
import scala.tools.nsc.tasty.{TastyUniverse, TastyModes, SafeEq}, TastyModes._
import scala.reflect.internal.MissingRequirementError

trait ContextOps { self: TastyUniverse =>
  import self.{symbolTable => u}, u.{internal => ui}

  private def describeOwner(owner: Symbol): String = {
    val kind =
      if (owner.is(Param)) {
        if (owner.isType) "type parameter"
        else "parameter"
      }
      else {
        owner.kindString
      }
    s"$kind ${owner.nameString}"
  }

  @inline final def unsupportedTermTreeError[T](noun: String)(implicit ctx: Context): T =
    unsupportedError(
      if (ctx.mode.is(ReadAnnotation)) s"$noun in an annotation of ${describeOwner(ctx.owner)}; note that complex trees are not yet supported for Annotations"
      else noun
    )

  @inline final def unsupportedError[T](noun: String)(implicit ctx: Context): T = {
    def location(owner: Symbol): String = {
      if (owner.isClass) s"${owner.kindString} ${owner.fullNameString}"
      else s"${describeOwner(owner)} in ${location(owner.owner)}"
    }
    typeError(s"Unsupported Scala 3 $noun; found in ${location(ctx.globallyVisibleOwner)}.")
  }

  @inline final def typeError[T](msg: String): T = throw new u.TypeError(msg)

  @inline final def assertError[T](msg: String): T =
    throw new AssertionError(s"assertion failed: ${u.supplementErrorMessage(msg)}")

  @inline final def assert(assertion: Boolean, msg: => Any): Unit =
    if (!assertion) assertError(String.valueOf(msg))

  @inline final def assert(assertion: Boolean): Unit =
    if (!assertion) assertError("")

  sealed abstract class Context {

    final def globallyVisibleOwner: Symbol = owner.logicallyEnclosingMember

    final def ignoreAnnotations: Boolean = u.settings.YtastyNoAnnotations

    final def adjustModuleClassCompleter(completer: TastyLazyType, name: TastyName): completer.type = {
      def findModule(name: TastyName, scope: u.Scope): Symbol = {
        val it = scope.lookupAll(encodeTermName(name)).filter(_.is(Object))
        if (it.hasNext) it.next()
        else noSymbol
      }

      /** Either empty scope, or, if the current context owner is a class,
       *  the declarations of the current class.
       */
      def effectiveScope(ctx: Context): u.Scope =
        if (ctx.owner != null && ctx.owner.isClass) ctx.owner.rawInfo.decls
        else u.EmptyScope

      completer.withSourceModule(ctx => findModule(name.toTermName, effectiveScope(ctx)))
    }

    final def log(str: => String): Unit = {
      if (u.settings.YdebugTasty)
        u.reporter.echo(
          pos = u.NoPosition,
          msg = str.linesIterator.map(line => s"#[$classRoot]: $line").mkString(System.lineSeparator)
        )
    }

    def owner: Symbol
    def source: AbstractFile
    def mode: TastyMode

    private final def loadingMirror: u.Mirror = u.mirrorThatLoaded(owner)

    final def requiredPackage(fullname: TastyName): Symbol = {
      if (fullname === TastyName.Root || fullname === TastyName.RootPkg) loadingMirror.RootPackage
      else if (fullname === TastyName.EmptyPkg) loadingMirror.EmptyPackage
      symOrDependencyError(false, true, fullname)(loadingMirror.getPackage(encodeTermName(fullname).toString))
    }

    final def requiredClass(fullname: TastyName.TypeName): Symbol =
      symOrDependencyError(false, false, fullname)(loadingMirror.getRequiredClass(encodeTypeName(fullname).toString))

    final def optionalClass(fullname: TastyName.TypeName): Option[Symbol] =
      loadingMirror.getClassIfDefined(encodeTypeName(fullname).toString).toOption

    final def requiredModule(fullname: TastyName.ModuleName): Symbol =
      symOrDependencyError(true, false, fullname)(loadingMirror.getRequiredModule(encodeTermName(fullname).toString))

    private def symOrDependencyError(isModule: Boolean, isPackage: Boolean, fullname: TastyName)(sym: => Symbol): Symbol = {
      try sym
      catch {
        case _: MissingRequirementError =>
          val kind = if (isModule) "object" else if (isPackage) "package" else "class"
          val addendum = if (mode.is(ReadAnnotation)) s" whilst reading annotation of $owner" else ""
          val msg =
            s"could not find $kind ${fullname.source}$addendum; perhaps it is missing from the classpath."
          typeError(msg)
      }
    }

    final lazy val classRoot: Symbol = initialContext.topLevelClass

    final def newLocalDummy: Symbol = owner.newLocalDummy(u.NoPosition)

    final def newWildcardSym(info: Type): Symbol =
      owner.newTypeParameter(u.nme.WILDCARD.toTypeName, u.NoPosition, u.NoFlags).setInfo(info)

    final def isSameRoot(root: Symbol, name: TastyName): Boolean = {
      val selector = encodeTastyName(name)
      (root.owner `eq` this.owner) && selector === root.name
    }

    final def findOuterClassTypeParameter(name: TastyName.TypeName): Symbol = {
      val selector: u.Name = encodeTypeName(name)
      owner.owner.typeParams.find(selector === _.name).getOrElse {
        throw new AssertionError(s"${owner.owner} has no type params.")
      }
    }

    final def newRefinementSymbol(parent: Type, owner: Symbol, name: TastyName, tpe: Type): Symbol = {
      val overridden = parent.member(encodeTastyName(name))
      val isOverride = isSymbol(overridden)
      var flags      = if (isOverride && overridden.isType) Override else EmptyTastyFlags
      val info = {
        if (name.isTermName) {
          flags |= Method | Deferred
          tpe match {
            case u.TypeRef(_, u.definitions.ByNameParamClass, arg :: Nil) => // nullary method
              ui.nullaryMethodType(arg)
            case u.PolyType(tparams, res) if res.paramss.isEmpty => ui.polyType(tparams, ui.nullaryMethodType(res))
            case _:u.MethodType | _:u.PolyType => tpe
            case _ => // val, which is not stable if structural. Dotty does not support vars
              if (isOverride && overridden.is(Stable)) flags |= Stable
              ui.nullaryMethodType(tpe)
          }
        }
        else {
          if (tpe.isInstanceOf[u.TypeBounds]) flags |= Deferred
          tpe
        }
      }
      newSymbol(owner, name, flags, info)
    }

    final def newSymbol(owner: Symbol, name: TastyName, flags: TastyFlagSet, info: Type, privateWithin: Symbol = noSymbol): Symbol =
      adjustSymbol(
        symbol = {
          if (flags.is(Param)) {
            if (name.isTypeName) {
              owner.newTypeParameter(encodeTypeName(name.toTypeName), u.NoPosition, encodeFlagSet(flags))
            }
            else {
              owner.newValueParameter(encodeTermName(name), u.NoPosition, encodeFlagSet(flags))
            }
          }
          else if (name === TastyName.Constructor) {
            owner.newConstructor(u.NoPosition, encodeFlagSet(flags &~ Stable))
          }
          else if (flags.is(Object)) {
            owner.newModule(encodeTermName(name), u.NoPosition, encodeFlagSet(flags))
          }
          else if (name.isTypeName) {
            owner.newTypeSymbol(encodeTypeName(name.toTypeName), u.NoPosition, encodeFlagSet(flags))
          }
          else {
            owner.newMethodSymbol(encodeTermName(name), u.NoPosition, encodeFlagSet(flags))
          }
        },
        info = info,
        privateWithin = privateWithin
      )

    final def newClassSymbol(owner: Symbol, typeName: TastyName.TypeName, flags: TastyFlagSet, completer: TastyLazyType, privateWithin: Symbol): ClassSymbol = {
      adjustSymbol(
        symbol = owner.newClassSymbol(encodeTypeName(typeName), u.NoPosition, encodeFlagSet(flags)),
        info = completer,
        privateWithin = privateWithin
      )
    }

    final def enterClassCompletion(): ClassSymbol = {
      val cls = globallyVisibleOwner.asClass
      val assumedSelfType =
        if (cls.is(Object) && cls.owner.isClass) defn.SingleType(cls.owner.thisType, cls.sourceModule)
        else u.NoType
      cls.info = ui.classInfoType(cls.completer.parents, cls.completer.decls, assumedSelfType.typeSymbolDirect)
      cls
    }

    /** Normalises the parents and sets up value class machinery */
    final def adjustParents(cls: Symbol, parents: List[Type]): List[Type] = {
      val parentTypes = parents.map { tp =>
        val tpe = tp.dealias
        if (tpe.typeSymbolDirect === u.definitions.ObjectClass) u.definitions.AnyRefTpe
        else tpe
      }
      if (parentTypes.head.typeSymbolDirect === u.definitions.AnyValClass) {
        // TODO [tasty]: please reconsider if there is some shared optimised logic that can be triggered instead.
        withPhaseNoLater("extmethods") { ctx0 =>
          // duplicated from scala.tools.nsc.transform.ExtensionMethods
          cls.primaryConstructor.makeNotPrivate(noSymbol)
          for (decl <- cls.info.decls if decl.isMethod) {
            if (decl.isParamAccessor) decl.makeNotPrivate(cls)
            if (!decl.isClassConstructor) {
              val extensionMeth = decl.newExtensionMethodSymbol(cls.companion, u.NoPosition)
              extensionMeth setInfo u.extensionMethInfo(cls, extensionMeth, decl.info, cls)
            }
          }
        }
      }
      parentTypes
    }

    final def removeFlags(symbol: Symbol, flags: TastyFlagSet): symbol.type =
      symbol.resetFlag(encodeFlagSet(flags))

    final def addFlags(symbol: Symbol, flags: TastyFlagSet): symbol.type =
      symbol.setFlag(encodeFlagSet(flags))

    final def adjustSymbol(symbol: Symbol, flags: TastyFlagSet, info: Type, privateWithin: Symbol): symbol.type =
      adjustSymbol(addFlags(symbol, flags), info, privateWithin)

    final def adjustSymbol(symbol: Symbol, info: Type, privateWithin: Symbol): symbol.type = {
      symbol.privateWithin = privateWithin
      symbol.info = info
      symbol
    }

    /** Determines the owner of a refinement in the current context by the following steps:
     *  1) if the owner if this context is a refinement symbol, we are in a recursive RefinedType. Ensure that the
     *     context owner is initialised with the parent and reuse it.
     *  2) if the parent is also a RefinedType, then we will flatten the nested structure by reusing its owner
     *  3) the parent is not a RefinedType, and we are not in an enclosing RefinedType, so create a new RefinementClassSymbol.
     *  The Parent alongside the RefinedType owner are passed to the given operation
     */
    final def enterRefinement[T](parent: Type)(op: Context => T): T = {
      val clazz = owner match {
        case enclosing: u.RefinementClassSymbol =>
          if (!enclosing.hasRawInfo) mkRefinedTypeWith(parent :: Nil, enclosing, u.newScope)
          enclosing
        case _ => parent match {
          case nested: u.RefinedType => nested.typeSymbol
          case _                     => newRefinementClassSymbol
        }
      }
      op(withOwner(clazz))
    }

    final def newRefinementClassSymbol: Symbol = owner.newRefinementClass(u.NoPosition)

    final def initialiseClassScope(clazz: Symbol): Unit =
      clazz.completer.withDecls(u.newScope)

    final def setInfo(sym: Symbol, info: Type): Unit = sym.info = info

    final def onCompletionError[T](sym: Symbol): PartialFunction[Throwable, T] = {
      case err: u.TypeError =>
        sym.info = u.ErrorType
        throw err
    }

    final def errorInContext: PartialFunction[Throwable, String] = {
      case err: u.TypeError =>
        owner.info = u.ErrorType
        err.getMessage()
    }

    @tailrec
    final def initialContext: InitialContext = this match {
      case ctx: InitialContext => ctx
      case ctx: FreshContext   => ctx.outer.initialContext
    }

    final def withOwner(owner: Symbol): Context =
      if (owner `ne` this.owner) fresh(owner) else this

    final def withNewScope: Context =
      fresh(newLocalDummy)

    final def selectionCtx(name: TastyName): Context = this // if (name.isConstructorName) this.addMode(Mode.InSuperCall) else this
    final def fresh(owner: Symbol): FreshContext = new FreshContext(owner, this, this.mode)

    private def sibling(mode: TastyMode): FreshContext = new FreshContext(this.owner, outerOrThis, mode)
    private def sibling: FreshContext = sibling(mode)

    private def outerOrThis: Context = this match {
      case ctx: FreshContext => ctx.outer
      case ctx               => ctx
    }

    final def addMode(mode: TastyMode): Context =
      if (!this.mode.is(mode)) sibling(this.mode | mode)
      else this

    final def retractMode(mode: TastyMode): Context =
      if (this.mode.isOneOf(mode)) sibling(this.mode &~ mode)
      else this

    final def withMode(mode: TastyMode): Context =
      if (mode != this.mode) sibling(mode)
      else this

    final def withSource(source: AbstractFile): Context =
      if (source `ne` this.source) sibling.atSource(source)
      else this

    final def withPhaseNoLater[T](phase: String)(op: Context => T): T =
      u.enteringPhaseNotLaterThan[T](u.findPhaseWithName(phase))(op(this))

    /** Enter a phase and apply an error handler if the current phase is after the one specified
      */
    final def withSafePhaseNoLater[E, T](phase: String)(pf: Context => PartialFunction[Throwable, E])(op: Context => T): Either[E, T] = {
      val phase0 = u.findPhaseWithName(phase)
      if (u.isAtPhaseAfter(phase0)) {
        try {
          u.enteringPhaseNotLaterThan(phase0)(Right(op(this)))
        } catch pf(this) andThen (Left(_))
      } else {
        Right(op(this))
      }
    }
  }

  final class InitialContext(val topLevelClass: Symbol, val source: AbstractFile) extends Context {
    def mode: TastyMode = EmptyTastyMode
    def owner: Symbol = topLevelClass.owner
  }

  final class FreshContext(val owner: Symbol, val outer: Context, val mode: TastyMode) extends Context {
    private[this] var mySource: AbstractFile = null
    def atSource(source: AbstractFile): this.type = { mySource = source ; this }
    def source: AbstractFile = if (mySource == null) outer.source else mySource
  }
}
