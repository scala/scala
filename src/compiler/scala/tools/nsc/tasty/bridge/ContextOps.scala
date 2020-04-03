package scala.tools.nsc.tasty.bridge

import scala.annotation.tailrec

import scala.collection.mutable
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

      def ignoreAnnotations: Boolean = u.settings.YtastyNoAnnotations

      def adjustModuleCompleter(completer: TastyLazyType, name: Name): completer.type = {
        val scope = this.effectiveScope
        if (name.isTermName)
          completer withModuleClass (_.findModuleBuddy(name.toTypeName, scope))
        else
          completer withSourceModule (_.findModuleBuddy(name.toTermName, scope))
      }

      private def findModuleBuddy(name: Name, scope: Scope): Symbol = {
        val it = scope.lookupAll(name).filter(_.is(Module))
        if (it.hasNext) it.next()
        else noSymbol
      }

      /** Either empty scope, or, if the current context owner is a class,
       *  the declarations of the current class.
       */
      def effectiveScope: Scope =
        if (owner != null && owner.isClass) owner.rawInfo.decls
        else emptyScope

      def requiredPackage(name: TermName): TermSymbol = loadingMirror.getPackage(name.toString)

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

      private[this] var _modules: mutable.AnyRefMap[TermName, ModuleSymbol] = null
      private[this] def modules = {
        if (_modules == null) _modules = mutable.AnyRefMap.empty
        _modules
      }

      final def loadingMirror: Mirror = mirrorThatLoaded(owner)

      final lazy val classRoot: Symbol = initialContext.topLevelClass

      def newLocalDummy: TermSymbol = owner.newLocalDummy(noPosition)

      def newWildcardSym(info: TypeBounds): Symbol =
        owner.newTypeParameter(nme.WILDCARD.toTypeName, noPosition, emptyFlags).setInfo(info)

      def newSymbol(owner: Symbol, name: Name, flags: FlagSet, info: Type, privateWithin: Symbol = noSymbol): Symbol = {
        val sym = {
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
            val moduleName = name.toTermName
            val moduleSym  = owner.newModule(moduleName, noPosition, flags)
            modules += moduleName -> moduleSym
            moduleSym
          }
          else if (name.isTypeName) {
            owner.newTypeSymbol(name.toTypeName, noPosition, flags)
          }
          else {
            owner.newMethodSymbol(name.toTermName, noPosition, flags)
          }
        }
        sym.privateWithin = privateWithin
        sym.info = info
        sym
      }

      def newClassSymbol(owner: Symbol, typeName: TypeName, flags: FlagSet, completer: TastyLazyType, privateWithin: Symbol): ClassSymbol = {
        val sym = owner.newClassSymbol(name = typeName, newFlags = flags.ensuring(Abstract, when = Trait))
        sym.privateWithin = privateWithin
        sym.info = completer
        sym
      }

      def moduleClassFor(moduleName: TermName, flags: FlagSet, completer: TastyLazyType, privateWithin: Symbol): Symbol = {
        val module = modules.remove(moduleName).getOrElse(throw new AssertionError(
          "unpickling module class from TASTy before its module val."))
        val moduleClass = module.moduleClass
        moduleClass.info = completer
        moduleClass.flags = flags
        moduleClass.privateWithin = privateWithin
        moduleClass
      }

      def newRefinedClassSymbol(coord: Position): RefinementClassSymbol = owner.newRefinementClass(coord)

      @tailrec
      final def initialContext: InitialContext = this match {
        case ctx: InitialContext => ctx
        case ctx: FreshContext   => ctx.outer.initialContext
      }

      final def withOwner(owner: Symbol): Context =
        if (owner `ne` this.owner) fresh(owner) else this

      final def withNewScope: Context =
        withOwner(newLocalDummy)

      final def selectionCtx(name: TastyName): Context = this // if (name.isConstructorName) this.addMode(Mode.InSuperCall) else this
      final def fresh(owner: Symbol): FreshContext = new FreshContext(owner, this, this.mode)
      final def fresh: FreshContext = new FreshContext(this.owner, this, this.mode)
      final def withMode(mode: TastyMode): FreshContext = new FreshContext(this.owner, this, this.mode | mode)
    }

    final class InitialContext(val topLevelClass: Symbol, val source: AbstractFile) extends Context {
      def mode: TastyMode = EmptyTastyMode
      def owner: Symbol = topLevelClass.owner
    }

    final class FreshContext(val owner: Symbol, val outer: Context, val mode: TastyMode) extends Context {
      def source: AbstractFile = outer.source
    }
  }
}
