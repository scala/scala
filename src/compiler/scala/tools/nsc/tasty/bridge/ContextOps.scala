package scala.tools.nsc.tasty.bridge

import scala.annotation.tailrec

import scala.reflect.io.AbstractFile
import scala.tools.nsc.tasty.TastyUniverse
import scala.tools.nsc.tasty.TastyName
import scala.tools.nsc.tasty.TastyModes._

trait ContextOps { self: TastyUniverse =>
  import self.{symbolTable => u}
  import FlagSets._
  import SymbolOps._

  object Contexts {

    sealed abstract class Context {

      @inline final def unsupportedError[T](noun: String): T =
        typeError(s"Unsupported Scala 3 $noun; found in ${owner.fullLocationString}.")

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

      final def loadingMirror: Mirror = mirrorThatLoaded(owner)

      final lazy val classRoot: Symbol = initialContext.topLevelClass

      final def newLocalDummy: TermSymbol = owner.newLocalDummy(noPosition)

      final def newWildcardSym(info: TypeBounds): Symbol =
        owner.newTypeParameter(nme.WILDCARD.toTypeName, noPosition, emptyFlags).setInfo(info)

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
}
