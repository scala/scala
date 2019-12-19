package scala.tools.nsc.tasty.bridge

import scala.annotation.tailrec

import scala.reflect.io.AbstractFile
import scala.tools.nsc.tasty.TastyUniverse

trait ContextOps extends TastyKernel { self: TastyUniverse =>
  import FlagSets._

  object Contexts {

    sealed abstract class Context {
      import SymbolOps._

      type ThisContext <: Context

      def adjustModuleCompleter(completer: TastyLazyType, name: Name): TastyLazyType = {
        val scope = this.effectiveScope
        if (name.isTermName)
          completer withModuleClass (implicit ctx => findModuleBuddy(name.toTypeName, scope))
        else
          completer withSourceModule (implicit ctx => findModuleBuddy(name.toTermName, scope))
      }

      private def findModuleBuddy(name: Name, scope: Scope)(implicit ctx: Context): Symbol = {
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

      final def log(str: => String): Unit = logTasty(s"#[${classRoot}]: $str")

      final def picklerPhase: Phase = symbolTable.picklerPhase
      final def extmethodsPhase: Phase = symbolTable.findPhaseWithName("extmethods")

      def owner: Symbol
      def source: AbstractFile

      def EmptyPackage: ModuleSymbol = loadingMirror.EmptyPackage
      def RootPackage: ModuleSymbol = loadingMirror.RootPackage

      final lazy val loadingMirror: Mirror = initialContext.baseLoadingMirror
      final lazy val classRoot: Symbol = initialContext.baseClassRoot

      def newLocalDummy(owner: Symbol): TermSymbol = owner.newLocalDummy(noPosition)

      def newSymbol(owner: Symbol, name: Name, flags: FlagSet, completer: TastyLazyType, privateWithin: Symbol = noSymbol): Symbol = {
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
            owner.newModule(name.toTermName, noPosition, flags)
          }
          else if (name.isTypeName) {
            owner.newTypeSymbol(name.toTypeName, noPosition, flags)
          }
          else {
            owner.newMethodSymbol(name.toTermName, noPosition, flags)
          }
        }
        sym.privateWithin = privateWithin
        sym.info = completer
        sym
      }

      def newClassSymbol(owner: Symbol, typeName: TypeName, flags: FlagSet, completer: TastyLazyType, privateWithin: Symbol): ClassSymbol = {
        val sym = owner.newClassSymbol(name = typeName, newFlags = flags.ensuring(Abstract, when = Trait))
        sym.privateWithin = privateWithin
        sym.info = completer
        sym
      }

      /** if isConstructor, make sure it has one non-implicit parameter list */
      def normalizeIfConstructor(termParamss: List[List[Symbol]], isConstructor: Boolean): List[List[Symbol]] =
        if (isConstructor &&
          (termParamss.isEmpty || termParamss.head.nonEmpty && termParamss.head.head.is(Implicit)))
          Nil :: termParamss
        else
          termParamss

      /** The given type, unless `sym` is a constructor, in which case the
       *  type of the constructed instance is returned
       */
      def effectiveResultType(sym: Symbol, typeParams: List[Symbol], givenTp: Type): Type =
        if (sym.name == nme.CONSTRUCTOR) mkTypeRef(sym.owner.toType.prefix, sym.owner, typeParams.map(_.tpe))
        else givenTp

      /** The method type corresponding to given parameters and result type */
      def methodType(typeParams: List[Symbol], valueParamss: List[List[Symbol]], resultType: Type, isJava: Boolean = false): Type = {
        if (isJava)
          valueParamss.foreach(vs => vs.headOption.foreach(v => assert(v.flags.not(Implicit))))
        val monotpe = valueParamss.foldRight(resultType)((ts, f) => mkMethodType(ts, f))
        val exprMonotpe = {
          if (valueParamss.nonEmpty)
            monotpe
          else
            mkNullaryMethodType(monotpe)
        }
        if (typeParams.nonEmpty)
          mkPolyType(typeParams, exprMonotpe)
        else
          exprMonotpe
      }

      @tailrec
      final def initialContext: InitialContext = this match {
        case ctx: InitialContext => ctx
        case ctx: FreshContext   => ctx.outer.initialContext
      }

      final def withOwner(owner: Symbol): Context =
        if (owner `ne` this.owner) fresh.setOwner(owner) else this

      final def fresh: FreshContext = new FreshContext(this)
    }

    final class InitialContext(val baseClassRoot: Symbol, val baseLoadingMirror: Mirror, val source: AbstractFile) extends Context {
      type ThisContext = InitialContext
      val owner: Symbol = baseClassRoot.owner
    }

    final class FreshContext(val outer: Context) extends Context {
      type ThisContext = FreshContext
      private[this] var _owner = outer.owner
      def source: AbstractFile = outer.source
      def owner: Symbol = _owner
      def setOwner(owner: Symbol): ThisContext = { _owner = owner; this }
    }

    final def withPhaseNoLater[T](otherPhase: scala.tools.nsc.Phase)(op: Context => T)(implicit ctx: Context): T = {
      if (!isNoPhase(otherPhase) && phase.id > otherPhase.id)
        enteringPhase(otherPhase) { op(ctx) }
      else
        op(ctx)
    }
  }
}
