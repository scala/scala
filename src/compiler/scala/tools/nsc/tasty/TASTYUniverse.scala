package scala.tools.nsc.tasty

import scala.reflect.internal._
import scala.reflect.io.AbstractFile
import scala.annotation.tailrec

trait TastyUniverse { self =>
  val symbolTable: SymbolTable

  import symbolTable._
  import TastyFlags.Live._
  import FlagSets._
  import Contexts._

  final implicit val symbolTablePrecise: self.symbolTable.type = self.symbolTable

  final def logTasty(str: => String): Unit = {
    import symbolTable._
    if (settings.debugTasty) reporter.echo(NoPosition, str)
  }

  type ParamSig = Signature.ParamSig[TypeName]
  type Sig      = Signature[TypeName]
  val  Sig      = Signature
  type SigName  = SignedName[TermName, TypeName]
  val  SigName  = SignedName

  object FlagSets {
    import scala.reflect.internal.{Flags, ModifierFlags}

    val Private: FlagSet = Flag.PRIVATE
    val Protected: FlagSet = Flag.PROTECTED
    val AbsOverride: FlagSet = Flag.ABSOVERRIDE
    val Abstract: FlagSet = Flag.ABSTRACT
    val Final: FlagSet = Flag.FINAL

    val Interface: FlagSet = Flag.INTERFACE
    val Sealed: FlagSet = Flag.SEALED
    val Case: FlagSet = Flag.CASE
    val Implicit: FlagSet = ModifierFlags.IMPLICIT
    val Lazy: FlagSet = Flag.LAZY
    val Override: FlagSet = Flag.OVERRIDE
    val Macro: FlagSet = Flag.MACRO
    val JavaStatic: FlagSet = ModifierFlags.STATIC
    val Module: FlagSet = Flags.MODULE
    val Trait: FlagSet = Flag.TRAIT
    val Enum: FlagSet = Flag.ENUM
    val Local: FlagSet = Flag.LOCAL
    val Synthetic: FlagSet = Flag.SYNTHETIC
    val Artifact: FlagSet = Flag.ARTIFACT
    val Mutable: FlagSet = Flag.MUTABLE
    val Accessor: FlagSet = Flags.ACCESSOR
    val CaseAccessor: FlagSet = Flag.CASEACCESSOR
    val Covariant: FlagSet = Flag.COVARIANT
    val Contravariant: FlagSet = Flag.CONTRAVARIANT
    val DefaultParameterized: FlagSet = Flag.DEFAULTPARAM
    val StableRealizable: FlagSet = Flag.STABLE
    val ParamAccessor: FlagSet = Flag.PARAMACCESSOR
    val Param: FlagSet = Flag.PARAM
    val Deferred: FlagSet = Flag.DEFERRED
    val Method: FlagSet = Flags.METHOD
    val ModuleVal: FlagSet = Flags.MODULEVAR // different encoding of objects than dotty

    val NoInitsInterface: (FlagSet, TastyFlagSet) = (Interface, NoInits)
    val TermParamOrAccessor: FlagSet = Param | ParamAccessor
    val ModuleValCreationFlags: FlagSet = ModuleVal | Lazy | Final | StableRealizable
    val ModuleClassCreationFlags: FlagSet = Flags.ModuleFlags | Final
    val DeferredOrLazyOrMethod: FlagSet = Deferred | Lazy | Method

    implicit class FlagSetOps(private val flagSet: FlagSet) {
      private def flags: FlagSet = {
        val fs = flagSet & phase.flagMask
        (fs | ((fs & Flags.LateFlags) >>> Flags.LateShift)) & ~((fs & Flags.AntiFlags) >>> Flags.AntiShift)
      }
      private def getFlag(mask: FlagSet): FlagSet = {
        mask & (if ((mask & Flags.PhaseIndependentFlags) == mask) flagSet else flags)
      }
      def not(mask: FlagSet): Boolean = getFlag(mask) == 0
      def is(mask: FlagSet): Boolean = getFlag(mask) != 0
      def isOneOf(mask: FlagSet): Boolean = is(mask)
    }
  }

  /**
   * Ported from dotc
   */
  abstract class TastyLazyType extends LazyType with FlagAgnosticCompleter { self =>
    private[this] val NoSymbolFn = (_: Context) => NoSymbol
    private[this] var myDecls: Scope = EmptyScope
    private[this] var mySourceModuleFn: Context => Symbol = NoSymbolFn
    private[this] var myModuleClassFn: Context => Symbol = NoSymbolFn
    private[this] var myTastyFlagSet: TastyFlagSet = EmptyFlags

    /** The type parameters computed by the completer before completion has finished */
    def completerTypeParams(sym: Symbol)(implicit ctx: Context): List[Symbol] = sym.info.typeParams
    //      if (sym.is(Touched)) Nil // return `Nil` instead of throwing a cyclic reference
    //      else sym.info.typeParams

    override def decls: Scope = myDecls
    def sourceModule(implicit ctx: Context): Symbol = mySourceModuleFn(ctx)
    def moduleClass(implicit ctx: Context): Symbol = myModuleClassFn(ctx)
    def tastyFlagSet: TastyFlagSet = myTastyFlagSet

    def withDecls(decls: Scope): this.type = { myDecls = decls; this }
    def withSourceModule(sourceModuleFn: Context => Symbol): this.type = { mySourceModuleFn = sourceModuleFn; this }
    def withModuleClass(moduleClassFn: Context => Symbol): this.type = { myModuleClassFn = moduleClassFn; this }
    def withTastyFlagSet(flags: TastyFlagSet): this.type = { myTastyFlagSet = flags; this }

    override def load(sym: Symbol): Unit = complete(sym)
  }

  object SymbolOps {
    implicit class SymbolDecorator(sym: Symbol) {
      def completer: TastyLazyType = {
        assert(sym.rawInfo.isInstanceOf[TastyLazyType], s"Expected TastyLazyType, is ${showRaw(sym.rawInfo)} ")
        sym.rawInfo.asInstanceOf[TastyLazyType]
      }
      def ensureCompleted(): Unit = sym.info
      def typeRef(args: List[Type]): Type = symbolTable.typeRef(sym.owner.toType, sym, args)
      def typeRef: Type = symbolTable.typeRef(sym.owner.toType, sym, Nil)
      def termRef: Type = symbolTable.typeRef(sym.owner.toType, sym, Nil)
      def safeOwner: Symbol = if (sym.owner eq sym) sym else sym.owner
      def isOneOf(mask: FlagSet): Boolean = sym.hasFlag(mask)
      def is(mask: FlagSet, butNot: FlagSet = NoFlags): Boolean =
        if (butNot == NoFlags)
          sym.hasFlag(mask)
        else
          sym.hasFlag(mask) && sym.hasNoFlags(butNot)
      def not(mask: FlagSet): Boolean = !is(mask)
    }
  }

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
        else NoSymbol
      }

      /** Either empty scope, or, if the current context owner is a class,
       *  the declarations of the current class.
       */
      def effectiveScope: Scope =
        if (owner != null && owner.isClass) owner.rawInfo.decls
        else EmptyScope

      def requiredPackage(name: TermName): TermSymbol = loadingMirror.getPackage(name.toString)

      final def log(str: => String): Unit = logTasty(s"#${self.hashCode.toHexString.take(4)}: $str")

      final def picklerPhase: Phase = symbolTable.picklerPhase

      def owner: Symbol
      def source: AbstractFile

      def EmptyPackage: ModuleSymbol = loadingMirror.EmptyPackage
      def RootPackage: ModuleSymbol = loadingMirror.RootPackage

      final lazy val loadingMirror: Mirror = initialContext.baseLoadingMirror
      final lazy val classRoot: Symbol = initialContext.baseClassRoot

      def newLocalDummy(owner: Symbol): TermSymbol = owner.newLocalDummy(NoPosition)

      def newSymbol(owner: Symbol, name: Name, flags: FlagSet, completer: TastyLazyType, privateWithin: Symbol = NoSymbol): Symbol = {
        val sym = {
          if (flags.is(Param)) {
            if (name.isTypeName) {
              owner.newTypeParameter(name.toTypeName, NoPosition, flags)
            }
            else {
              owner.newValueParameter(name.toTermName, NoPosition, flags)
            }
          }
          else if (name == nme.CONSTRUCTOR) {
            owner.newConstructor(NoPosition, flags & ~Flag.STABLE)
          }
          else {
            owner.newMethodSymbol(name.toTermName, NoPosition, flags) // TODO: other kinds of symbols
          }
        }
        sym.setPrivateWithin(privateWithin)
        sym.info = completer
        sym
      }

      def newClassSymbol(owner: Symbol, typeName: TypeName, flags: FlagSet, completer: TastyLazyType, privateWithin: Symbol): ClassSymbol = {
        val sym = owner.newClassSymbol(name = typeName, newFlags = flags)
        sym.setPrivateWithin(privateWithin)
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
        if (sym.name == nme.CONSTRUCTOR) sym.owner.typeRef(typeParams.map(_.tpe))
        else givenTp

      /** The method type corresponding to given parameters and result type */
      def methodType(typeParams: List[Symbol], valueParamss: List[List[Symbol]], resultType: Type, isJava: Boolean = false): Type = {
        if (isJava)
          valueParamss.foreach(vs => vs.headOption.foreach(v => assert(v.flags.not(Implicit))))
        val monotpe = valueParamss.foldRight(resultType)((ts, f) => internal.methodType(ts, f))
        val exprMonotpe = {
          if (valueParamss.nonEmpty)
            monotpe
          else
            internal.nullaryMethodType(monotpe)
        }
        if (typeParams.nonEmpty)
          internal.polyType(typeParams, exprMonotpe)
        else
          exprMonotpe
      }

      @tailrec
      final def initialContext: InitialContext = this match {
        case ctx: InitialContext => ctx
        case ctx: FreshContext => ctx.outer.initialContext
      }

      final def withOwner(owner: Symbol): Context =
        if (owner ne this.owner) fresh.setOwner(owner) else this

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
      if ((otherPhase ne NoPhase) && phase.id > otherPhase.id)
        enteringPhase(otherPhase) { op(ctx) }
      else
        op(ctx)
    }
  }

  object NameOps {
    implicit class NameDecorator(name: Name) {
      def isConstructorName: Boolean = symbolTable.nme.isConstructorName(name)
    }
  }

  object Trees {
    /** A base trait for lazy tree fields.
     *  These can be instantiated with Lazy instances which
     *  can delay tree construction until the field is first demanded.
     */
    trait Lazy[+T <: AnyRef] {
      def complete(implicit ctx: Context): T
    }
  }

  abstract class LambdaTypeCompanion[N <: Name, PInfo <: Type, LT <: LambdaType] {
    def apply(paramNames: List[N])(paramInfosExp: LT => List[PInfo], resultTypeExp: LT => Type)(implicit ctx: Context): LT
  }

  abstract class LambdaType extends Type with Product {
    type ThisName <: Name
    type PInfo <: Type

    def paramNames: List[ThisName]
    def paramInfos: List[PInfo]
    def resType: Type

    private[this] var myParamRefs: List[TypeParamRef] = null

    def paramRefs: List[TypeParamRef] = {
      if (myParamRefs == null) myParamRefs = paramNames.indices.toList.map(i => new TypeParamRef(this, i))
      myParamRefs
    }

    override def safeToString(): String = {
      val args = paramNames.zip(paramInfos).map {
        case (name, info) => s"${name}$info"
      }.mkString("[", ", ", "]")
      s"$args =>> $resType"
    }

    override def typeParams: List[Symbol] = {
      if (myTypeParams `eq` null) myTypeParams = paramNames.zip(paramInfos).map {
        case (name, info) => newFreeTypeSymbol(name.toTypeName, Param | Deferred, name.toString).setInfo(info)
      }
      myTypeParams
    }

    /**Best effort to transform this to an equivalent canonical representation in scalac.
     */
    def asReflectType(owner: Symbol): Type =
      if (resType.typeArgs == paramRefs) resType.typeConstructor
      else if (resType.typeArgs.isEmpty) {
        internal.polyType(
          paramNames.zip(paramInfos).map {
            case (name, info) => owner.newTypeParameter(name.toTypeName, NoPosition, Param | Deferred).setInfo(info)
          },
          if (resType.typeSymbolDirect == definitions.AnyClass)
            TypeBounds.empty
          else
            resType.bounds
        )
      }
      else this

    def productArity: Int = 2
    def productElement(n: Int): Any = n match {
      case 0 => paramNames
      case 1 => resType
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    def canEqual(that: Any): Boolean = that.isInstanceOf[LambdaType]
  }

  final class TypeParamRef(binder: LambdaType, i: Int) extends Type with Product {

    override def safeToString(): String = binder.paramNames(i).toString()

    override val productPrefix: String = "TypeParamRef"
    val productArity = 1
    def productElement(n: Int): Any = n match {
      case 0 => binder.paramNames(i)
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    def canEqual(that: Any): Boolean = that.isInstanceOf[TypeParamRef]
  }

  object HKTypeLambda extends LambdaTypeCompanion[TypeName, TypeBounds, HKTypeLambda] {
    def apply(paramNames: List[TypeName])(
        paramInfosExp: HKTypeLambda => List[TypeBounds], resultTypeExp: HKTypeLambda => Type)(
        implicit ctx: Context): HKTypeLambda =
      new HKTypeLambda(paramNames)(paramInfosExp, resultTypeExp)
  }

  class HKTypeLambda(val paramNames: List[TypeName])(
      paramInfosExp: HKTypeLambda => List[TypeBounds], resultTypeExp: HKTypeLambda => Type)(implicit ctx: Context)
  extends LambdaType {
    type ThisName = TypeName
    type PInfo = TypeBounds

    override val productPrefix = "HKTypeLambda"
    val paramInfos: List[TypeBounds] = paramInfosExp(this)
    val resType: Type                = resultTypeExp(this)

    assert(resType.isComplete, this)
    assert(paramNames.nonEmpty)
  }

  def TypeRef(tpe: Type, name: Name): Type = {
    val symName = if (tpe.members.containsName(name)) name else name.encode
    typeRef(tpe, tpe.member(symName), Nil)
  }

  def showSym(sym: Symbol): String = s"$sym # ${sym.hashCode}"
}
