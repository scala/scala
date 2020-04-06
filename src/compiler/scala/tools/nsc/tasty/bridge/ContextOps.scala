package scala.tools.nsc.tasty.bridge

import scala.annotation.tailrec

import scala.reflect.io.AbstractFile
import scala.tools.nsc.tasty.TastyUniverse
import scala.tools.nsc.tasty.TastyName
import scala.tools.nsc.tasty.TastyModes._

trait ContextOps { self: TastyUniverse =>
  import self.{symbolTable => u}
  import FlagSets._

  object defn {
    final val AnyTpe: Type = u.definitions.AnyTpe
    final val NothingTpe: Type = u.definitions.NothingTpe
    final val AnyRefTpe: Type = u.definitions.AnyRefTpe
    final val UnitTpe: Type = u.definitions.UnitTpe
    final val JavaEnumClass: ClassSymbol = u.definitions.JavaEnumClass
    final val CompileTimeOnlyAttr: Symbol = u.definitions.CompileTimeOnlyAttr
    final val ByNameParamClass: ClassSymbol = u.definitions.ByNameParamClass
    final val ObjectClass: ClassSymbol = u.definitions.ObjectClass
    final val AnyValClass: ClassSymbol = u.definitions.AnyValClass
    final val ScalaPackage: ModuleSymbol = u.definitions.ScalaPackage
    final val TailrecClass: ClassSymbol = u.definitions.TailrecClass
    final val StaticAnnotationClass: ClassSymbol = u.definitions.StaticAnnotationClass
    final val SeqClass: ClassSymbol = u.definitions.SeqClass
    @inline final def byNameType(arg: Type): Type = u.definitions.byNameType(arg)
    @inline final def scalaRepeatedType(arg: Type): Type = u.definitions.scalaRepeatedType(arg)
    @inline final def repeatedAnnotationClass(implicit ctx: Context): Option[Symbol] = ctx.loadingMirror.getClassIfDefined("scala.annotation.internal.Repeated").toOption
    @inline final def childAnnotationClass(implicit ctx: Context): Option[Symbol] = ctx.loadingMirror.getClassIfDefined("scala.annotation.internal.Child").toOption
    @inline final def arrayType(dims: Int, arg: Type): Type = (0 until dims).foldLeft(arg)((acc, _) => u.definitions.arrayType(acc))
  }

  def picklerPhase: Phase = u.picklerPhase
  def namer: Phase = u.findPhaseWithName("namer")
  def extmethodsPhase: Phase = u.findPhaseWithName("extmethods")

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

    final def adjustModuleClassCompleter(completer: TastyLazyType, name: Name): completer.type = {
      def findModule(name: Name, scope: Scope): Symbol = {
        val it = scope.lookupAll(name).filter(_.is(Module))
        if (it.hasNext) it.next()
        else noSymbol
      }
      completer.withSourceModule(ctx => findModule(name.toTermName, ctx.effectiveScope))
    }

    /** Either empty scope, or, if the current context owner is a class,
     *  the declarations of the current class.
     */
    final def effectiveScope: Scope =
      if (owner != null && owner.isClass) owner.rawInfo.decls
      else emptyScope

    final def requiredPackage(name: TermName): TermSymbol = loadingMirror.getPackage(name.toString)

    final def log(str: => String): Unit = {
      if (u.settings.YdebugTasty)
        u.reporter.echo(
          pos = noPosition,
          msg = str.linesIterator.map(line => s"#[$classRoot]: $line").mkString(System.lineSeparator)
        )
    }

    def owner: Symbol
    def source: AbstractFile
    def mode: TastyMode

    final def loadingMirror: Mirror = u.mirrorThatLoaded(owner)

    final lazy val classRoot: Symbol = initialContext.topLevelClass

    final def newLocalDummy: TermSymbol = owner.newLocalDummy(noPosition)

    final def newWildcardSym(info: TypeBounds): Symbol =
      owner.newTypeParameter(u.nme.WILDCARD.toTypeName, noPosition, emptyFlags).setInfo(info)

    final def newSymbol(owner: Symbol, name: Name, flags: FlagSet, info: Type, privateWithin: Symbol = noSymbol): Symbol =
      adjustSymbol(
        symbol = {
          if (flags.is(Param)) {
            if (name.isTypeName) {
              owner.newTypeParameter(name.toTypeName, noPosition, flags)
            }
            else {
              owner.newValueParameter(name.toTermName, noPosition, flags)
            }
          }
          else if (name == nme.CONSTRUCTOR) {
            owner.newConstructor(noPosition, flags & ~Stable)
          }
          else if (flags.is(Module)) {
            owner.newModule(name.toTermName, noPosition, flags)
          }
          else if (name.isTypeName) {
            owner.newTypeSymbol(name.toTypeName, noPosition, flags)
          }
          else {
            owner.newMethodSymbol(name.toTermName, noPosition, flags)
          }
        },
        info = info,
        privateWithin = privateWithin
      )

    final def newClassSymbol(owner: Symbol, typeName: TypeName, flags: FlagSet, completer: TastyLazyType, privateWithin: Symbol): ClassSymbol = {
      adjustSymbol(
        symbol = owner.newClassSymbol(name = typeName, newFlags = flags.ensuring(Abstract, when = Trait)),
        info = completer,
        privateWithin = privateWithin
      )
    }

    final def adjustSymbol(symbol: Symbol, flags: FlagSet, info: Type, privateWithin: Symbol): symbol.type =
      adjustSymbol(symbol.setFlag(flags), info, privateWithin)

    final def adjustSymbol(symbol: Symbol, info: Type, privateWithin: Symbol): symbol.type = {
      symbol.privateWithin = privateWithin
      symbol.info = info
      symbol
    }

    final def newRefinedClassSymbol(coord: Position): RefinementClassSymbol = owner.newRefinementClass(coord)

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
    final def fresh: FreshContext = new FreshContext(this.owner, this, this.mode)

    final def addMode(mode: TastyMode): Context =
      if (!this.mode.is(mode)) new FreshContext(this.owner, this, this.mode | mode)
      else this

    final def withMode(mode: TastyMode): Context =
      if (mode != this.mode) new FreshContext(this.owner, this, mode)
      else this

    final def withSource(source: AbstractFile): Context =
      if (source `ne` this.source) fresh.atSource(source)
      else this

    final def withPhaseNoLater[T](phase: Phase)(op: Context => T): T = u.enteringPhaseNotLaterThan[T](phase)(op(this))

    /** Enter a phase and apply an error handler if the current phase is after the one specified
      */
    final def withSafePhaseNoLater[E, T](phase: Phase)(pf: PartialFunction[Throwable, E])(op: Context => T): Either[E, T] =
      if (u.isAtPhaseAfter(phase)) {
        try {
          u.enteringPhaseNotLaterThan(phase)(Right(op(this)))
        } catch pf andThen (Left(_))
      } else {
        Right(op(this))
      }

    final def currentPhase: Phase = u.phase

    @inline final def mkScope(syms: Symbol*): Scope = u.newScopeWith(syms:_*)
    def mkScope: Scope = u.newScope
    def emptyScope: Scope = u.EmptyScope
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
