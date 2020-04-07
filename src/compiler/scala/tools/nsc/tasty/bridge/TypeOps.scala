package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyFlags.TastyFlagSet
import scala.tools.nsc.tasty.TastyUniverse
import scala.tools.nsc.tasty.TastyName, TastyName.TypeName
import scala.tools.nsc.tasty.TastyModes._

import scala.tools.nsc.tasty._
import scala.reflect.internal.Variance
import scala.util.chaining._

import scala.collection.mutable

trait TypeOps { self: TastyUniverse =>
  import self.{symbolTable => u}, u.{internal => ui}
  import FlagSets._

  @inline final def mergeableParams(t: Type, u: Type): Boolean =
    t.typeParams.size == u.typeParams.size

  @inline final def unionIsUnsupported[T](implicit ctx: Context): T = unsupportedError(s"union in bounds of ${ctx.owner}")
  @inline final def matchTypeIsUnsupported[T](implicit ctx: Context): T = unsupportedError(s"match type in bounds of ${ctx.owner}")
  @inline final def erasedRefinementIsUnsupported[T](implicit ctx: Context): T = unsupportedError(s"erased modifier in refinement of ${ctx.owner}")

  @inline final def isConstantType(tpe: Type): Boolean = tpe.isInstanceOf[u.ConstantType]

  def errorType: Type = u.ErrorType
  def noType: Type = u.NoType
  def noPrefix: Type = u.NoPrefix

  @inline final def isError(tpe: Type): Boolean = tpe `eq` u.ErrorType
  @inline final def isNoType(tpe: Type): Boolean = tpe `eq` u.NoType

  object UnmergablePolyBounds {
    def unapply(tpe: TypeBounds): Boolean = tpe match {
      case u.TypeBounds(lo: PolyType, hi: PolyType) => !mergeableParams(lo,hi)
      case _                                        => false
    }
  }

  def fnResult(fn: Type): Type = fn.dealiasWiden.finalResultType
  def tyconResult(tycon: Type, args: List[Type]): Type = tycon.resultType.substituteTypes(tycon.typeParams, args)

  def emptyTypeBounds: TypeBounds = u.TypeBounds.empty

  @inline final def mkTypeBounds(lo: Type, hi: Type): TypeBounds = u.TypeBounds.apply(lo, hi)
  @inline final def mkSingleType(pre: Type, sym: Symbol): Type = u.singleType(pre, sym)
  @inline final def mkNullaryMethodType(res: Type): NullaryMethodType = ui.nullaryMethodType(res)
  @inline final def mkPolyType(params: List[Symbol], res: Type): PolyType = ui.polyType(params, res)
  @inline final def mkClassInfoType(parents: List[Type], decls: Scope, sym: Symbol): ClassInfoType = ui.classInfoType(parents, decls, sym)
  @inline final def mkThisType(sym: Symbol): Type = ui.thisType(sym)
  @inline final def mkConstantType(c: Constant): ConstantType = ui.constantType(c)
  @inline final def mkIntersectionType(tps: Type*): Type = ui.intersectionType(tps.toList)
  @inline final def mkIntersectionType(tps: List[Type]): Type = ui.intersectionType(tps)
  @inline final def mkAnnotatedType(tpe: Type, annot: Annotation): AnnotatedType = u.AnnotatedType(annot :: Nil, tpe)
  @inline final def mkRefinedType(parents: List[Type], clazz: Symbol): RefinedType = mkRefinedTypeWith(parents, clazz, u.newScope)
  @inline final def mkSuperType(thisTpe: Type, superTpe: Type): Type = u.SuperType(thisTpe, superTpe)
  @inline final def mkLambdaPolyType(typeParams: List[Symbol], resTpe: Type): LambdaPolyType = new LambdaPolyType(typeParams, resTpe)
  @inline final def mkLambdaFromParams(typeParams: List[Symbol], ret: Type): PolyType = ui.polyType(typeParams, lambdaResultType(ret))
  def mkRecType(run: RecType => Type)(implicit ctx: Context): Type = new RecType(run).parent

  private[bridge] def mkRefinedTypeWith(parents: List[Type], clazz: Symbol, decls: Scope): RefinedType =
    u.RefinedType.apply(parents, decls, clazz).tap(clazz.info = _)

  def refinedType(parent: Type, clazz: Symbol, name: TastyName, tpe: Type)(implicit ctx: Context): Type = {
    val decl = ctx.newRefinementSymbol(parent, clazz, name, tpe)
    parent match {
      case nested: RefinedType =>
        mkRefinedTypeWith(nested.parents, clazz, nested.decls.cloneScope.tap(_.enter(decl)))
      case _ =>
        mkRefinedTypeWith(parent :: Nil, clazz, ctx.mkScope(decl))
    }
  }


  /** The method type corresponding to given parameters and result type */
  def mkDefDefType(typeParams: List[Symbol], valueParamss: List[List[Symbol]], resultType: Type): Type = {
    val monotpe = valueParamss.foldRight(resultType)((ts, f) => ui.methodType(ts, f))
    val exprMonotpe = {
      if (valueParamss.nonEmpty)
        monotpe
      else
        ui.nullaryMethodType(monotpe)
    }
    if (typeParams.nonEmpty)
      ui.polyType(typeParams, exprMonotpe)
    else
      exprMonotpe
  }

  @inline final def extensionMethInfo(currentOwner: Symbol, extensionMeth: Symbol, origInfo: Type, clazz: Symbol): Type =
    u.extensionMethInfo(currentOwner, extensionMeth, origInfo, clazz)

  def normaliseBounds(bounds: TypeBounds): Type = {
    val u.TypeBounds(lo, hi) = bounds
    if (lo.isHigherKinded && hi.isHigherKinded) {
      if (mergeableParams(lo, hi)) {
        val nuLo = lo.resultType.upperBound.subst(lo.typeParams, hi.typeParams.map(_.ref))
        lo.typeParams.foreach { sym =>
          sym.owner.rawInfo.decls.unlink(sym)
          sym.owner.rawInfo.members.unlink(sym)
          sym.owner = noSymbol
        }
        ui.polyType(hi.typeParams, u.TypeBounds(nuLo, hi.resultType.upperBound))
      }
      else bounds match {
        case u.TypeBounds(lo: LambdaPolyType, hi: LambdaPolyType) => u.TypeBounds(lo.toNested,hi.toNested)
        case _                                                    => bounds
      }
    }
    else if (hi.isHigherKinded)
      ui.polyType(hi.typeParams, u.TypeBounds(lo.upperBound, hi.resultType.upperBound))
    else if (lo.isHigherKinded)
      ui.polyType(lo.typeParams, u.TypeBounds(lo.resultType.upperBound, hi.upperBound))
    else
      bounds
  }

  def boundedAppliedType(tycon: Type, args: List[Type])(implicit ctx: Context): Type = {

    def typeRefUncurried(tycon: Type, args: List[Type]): Type = tycon match {
      case tycon: TypeRef if tycon.typeArgs.nonEmpty =>
        unsupportedError(s"curried type application $tycon[${args.mkString(",")}]")
      case _ =>
        u.appliedType(tycon, args)
    }

    if (args.exists(tpe => tpe.isInstanceOf[TypeBounds] | tpe.isInstanceOf[LambdaPolyType])) {
      val syms = mutable.ListBuffer.empty[Symbol]
      def bindWildcards(tpe: Type) = tpe match {
        case tpe: TypeBounds     => ctx.newWildcardSym(tpe).tap(syms += _).pipe(_.ref)
        case tpe: LambdaPolyType => tpe.toNested
        case tpe                 => tpe
      }
      val args1 = args.map(bindWildcards)
      if (syms.isEmpty) typeRefUncurried(tycon, args1)
      else ui.existentialType(syms.toList, typeRefUncurried(tycon, args1))
    }
    else {
      typeRefUncurried(tycon, args)
    }

  }

  def resolveErasedTypeRef(ref: ErasedTypeRef)(implicit ctx: Context): Type = {
    val sym =
      if (ref.isModule) ctx.loadingMirror.getModuleIfDefined(ref.qualifiedName)
      else ctx.loadingMirror.getClassIfDefined(ref.qualifiedName)
    if (!isSymbol(sym)) {
      val kind = if (ref.isModule) "object" else "class"
      val addendum = if (ctx.mode.is(ReadAnnotation)) s" whilst reading annotation of ${ctx.owner}" else ""
      val msg =
        s"could not find $kind ${ref.qualifiedName}$addendum; perhaps it is missing from the classpath."
      typeError(msg)
    }
    defn.arrayType(ref.arrayDims, sym.tpe.erasure)
  }

  /** A type which accepts two type arguments, representing an intersection type
   * @see https://github.com/lampepfl/dotty/issues/7688
   */
  case object AndType extends Type

  def selectType(name: TypeName, prefix: Type)(implicit ctx: Context): Type = selectType(name, prefix, prefix)
  def selectType(name: TypeName, prefix: Type, space: Type)(implicit ctx: Context): Type = {
    if (prefix.typeSymbol === defn.ScalaPackage && ( name === tpnme.And || name === tpnme.Or ) ) {
      if (name === tpnme.And) AndType
      else unionIsUnsupported
    }
    else {
      namedMemberOfTypeWithPrefix(prefix, space, name)
    }
  }

  def selectTerm(name: TastyName, prefix: Type)(implicit ctx: Context): Type = selectTerm(name, prefix, prefix)
  def selectTerm(name: TastyName, prefix: Type, space: Type)(implicit ctx: Context): Type =
    namedMemberOfTypeWithPrefix(prefix, space, name.toTermName)

  def singletonLike(tpe: Type): Symbol = tpe match {
    case u.SingleType(_, sym) => sym
    case u.TypeRef(_,sym,_)   => sym
  }

  private[this] val NoSymbolFn = (_: Context) => noSymbol

  /**
   * Ported from dotc
   */
  abstract class TastyLazyType extends u.LazyType with u.FlagAgnosticCompleter {
    private[this] var myDecls: Scope = u.EmptyScope
    private[this] var mySourceModuleFn: Context => Symbol = NoSymbolFn
    private[this] var myTastyFlagSet: TastyFlagSet = emptyTastyFlags

    override def decls: Scope = myDecls
    def sourceModule(implicit ctx: Context): Symbol = mySourceModuleFn(ctx)
    def tastyFlagSet: TastyFlagSet = myTastyFlagSet

    def withDecls(decls: Scope): this.type = { myDecls = decls; this }
    def withSourceModule(sourceModuleFn: Context => Symbol): this.type = { mySourceModuleFn = sourceModuleFn; this }
    def withTastyFlagSet(flags: TastyFlagSet): this.type = { myTastyFlagSet = flags; this }

    override def load(sym: Symbol): Unit = complete(sym)
  }

  def prefixedRef(prefix: Type, sym: Symbol): Type = {
    if (sym.isType) {
      prefix match {
        case tp: ThisType if tp.sym.isRefinementClass => sym.preciseRef(prefix)
        case _:SingleType | _:RefinedType             => sym.preciseRef(prefix)
        case _                                        => sym.ref
      }
    }
    else if (sym.isConstructor) {
      normaliseConstructorRef(sym)
    }
    else if (sym.is(JavaStatic)) {
      // With this constraint, we avoid making singleton types for
      //  static forwarders to modules (or you get a stack overflow trying to get sealedDescendents in patmat)
      sym.preciseRef(prefix)
    }
    else {
      u.singleType(prefix, sym)
    }
  }

  def normaliseConstructorRef(ctor: Symbol): Type = {
    var tpe = ctor.tpe
    val tParams = ctor.owner.typeParams
    if (tParams.nonEmpty) tpe = ui.polyType(tParams, tpe)
    tpe
  }

  def namedMemberOfPrefix(pre: Type, name: TastyName)(implicit ctx: Context): Type =
    namedMemberOfTypeWithPrefix(pre, pre, name)

  def namedMemberOfTypeWithPrefix(pre: Type, space: Type, tname: TastyName)(implicit ctx: Context): Type =
    prefixedRef(pre, namedMemberOfType(space, tname))

  def lambdaResultType(resType: Type): Type = resType match {
    case res: LambdaPolyType => res.toNested
    case res                 => res
  }

  abstract class LambdaTypeCompanion[N <: TastyName, PInfo <: Type, LT <: LambdaType] {
    def factory(params: List[N])(registerCallback: LT => Unit, paramInfosOp: () => List[PInfo], resultTypeOp: () => Type)(implicit ctx: Context): LT

    final def apply(params: List[N])(registerCallback: LT => Unit, paramInfosOp: () => List[PInfo], resultTypeOp: () => Type)(implicit ctx: Context): Type =
      factory(params)(registerCallback, paramInfosOp, resultTypeOp).canonical
  }

  final class LambdaPolyType(typeParams: List[Symbol], resType: Type) extends PolyType(typeParams, LambdaPolyType.addLower(resType)) {
    def toNested: PolyType = resType match {
      case _: TypeBounds => this
      case _             => ui.polyType(typeParams, resType)
    }
    def withVariances(variances: List[Variance]): this.type = {
      typeParams.lazyZip(variances).foreach { (sym, variance) => // TODO [tasty]: should this be cloned instead?
        variance match {
          case Variance.Covariant => sym.flags |= Covariant
          case Variance.Contravariant => sym.flags |= Contravariant
          case _ => ()
        }
      }
      this
    }
  }

  object LambdaPolyType {
    private def addLower(tpe: Type): TypeBounds = tpe match {
      case tpe: TypeBounds => tpe
      case tpe             => u.TypeBounds.upper(tpe)
    }
  }

  def typeRef(tpe: Type): Type = u.appliedType(tpe, Nil)

  /** The given type, unless `sym` is a constructor, in which case the
   *  type of the constructed instance is returned
   */
  def effectiveResultType(sym: Symbol, typeParams: List[Symbol], givenTp: Type): Type =
    if (sym.name == u.nme.CONSTRUCTOR) sym.owner.tpe
    else givenTp

  type LambdaType = Type with Lambda
  type TypeLambda = LambdaType with TypeLike
  type TermLambda = LambdaType with TermLike

  trait TypeLike { self: Type with Lambda =>
    type ThisTName = TypeName
    type ThisName  = u.TypeName
    type PInfo = TypeBounds
  }

  trait TermLike { self: Type with Lambda =>
    type ThisTName = TastyName
    type ThisName  = u.TermName
    type PInfo = Type
  }

  trait Lambda extends Product with Serializable { self: Type =>
    type ThisTName <: TastyName
    type ThisName <: u.Name
    type PInfo <: Type
    type This <: Type

    val paramNames: List[ThisName]
    val paramInfos: List[PInfo]
    val resType: Type

    def typeParams: List[Symbol] // deferred to final implementation

    final protected def validateThisLambda(): Unit = {
      assert(resType.isComplete, self)
      assert(paramInfos.length == paramNames.length, self)
    }

    override final def productArity: Int = 2

    override final def productElement(n: Int): Any = n match {
      case 0 => paramNames
      case 1 => resType
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }

    def canEqual(that: Any): Boolean = that.isInstanceOf[Lambda]

    def canonical: This

    override final def equals(that: Any): Boolean = that match {
      case that: Lambda =>
        (that.canEqual(self)
          && that.paramNames == paramNames
          && that.resType    == resType)
      case _ => false
    }
  }

  object HKTypeLambda extends TypeLambdaCompanion[HKTypeLambda] {
    def factory(params: List[TypeName])(registerCallback: HKTypeLambda => Unit,
        paramInfosOp: () => List[TypeBounds], resultTypeOp: () => Type)(implicit ctx: Context): HKTypeLambda =
      new HKTypeLambda(params)(registerCallback, paramInfosOp, resultTypeOp)
  }

  object PolyType extends TypeLambdaCompanion[PolyTypeLambda] {
    def factory(params: List[TypeName])(registerCallback: PolyTypeLambda => Unit,
         paramInfosOp: () => List[TypeBounds], resultTypeOp: () => Type)(implicit ctx: Context): PolyTypeLambda =
      new PolyTypeLambda(params)(registerCallback, paramInfosOp, resultTypeOp)
  }

  abstract class MethodTypeCompanion(defaultFlags: FlagSet) extends TermLambdaCompanion[MethodTermLambda] { self =>
    def factory(params: List[TastyName])(registerCallback: MethodTermLambda => Unit,
        paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context): MethodTermLambda =
      new MethodTermLambda(params, defaultFlags)(registerCallback, paramInfosOp, resultTypeOp)
  }

  final class RecType(run: RecType => Type)(implicit ctx: Context) extends Type with Product {
    override val productPrefix = "RecType"
    override val productArity = 2

    val refinementClass = ctx.newRefinementClassSymbol(noPosition).setInfo(EmptyRecTypeInfo)
    val recThis: Type   = ui.thisType(refinementClass)
    val parent: Type    = run(this)

    def canEqual(that: Any): Boolean = that.isInstanceOf[RecType]
    def productElement(n: Int): Any = n match {
      case 0 => if (parent == null) "<under-construction>" else parent
      case 1 => hashCode
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }

    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    override def safeToString: String = s"RecType(rt @ $hashCode => ${if (parent == null) "<under-construction>" else parent})"

  }

  case object EmptyRecTypeInfo extends Type {
    override def isTrivial: Boolean = true
  }

  object MethodType extends MethodTypeCompanion(emptyFlags)
  object ImplicitMethodType extends MethodTypeCompanion(Implicit)

  abstract class TermLambdaCompanion[LT <: TermLambda]
    extends LambdaTypeCompanion[TastyName, Type, LT]

  abstract class TypeLambdaCompanion[LT <: TypeLambda]
    extends LambdaTypeCompanion[TypeName, TypeBounds, LT]

  final class MethodTermLambda(paramTNames: List[TastyName], defaultFlags: FlagSet)(registerCallback: MethodTermLambda => Unit,
    paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context)
  extends Type with Lambda with TermLike { methodLambda =>
    type This = MethodType

    val paramNames: List[u.TermName] = paramTNames.map(encodeTermName)

    override val productPrefix = "MethodTermLambda"

    registerCallback(this)

    val paramInfos: List[Type] = paramInfosOp()

    override val params: List[Symbol] = paramNames.lazyZip(paramInfos).map {
      case (name, argInfo) => ctx.owner.newValueParameter(name, noPosition, defaultFlags).setInfo(argInfo)
    }

    val resType: Type = resultTypeOp()

    validateThisLambda()

    def canonical: MethodType = ui.methodType(params, resType)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[MethodTermLambda]
  }

  final class HKTypeLambda(paramTNames: List[TypeName])(registerCallback: HKTypeLambda => Unit,
    paramInfosOp: () => List[TypeBounds], resultTypeOp: () => Type)(implicit ctx: Context)
  extends Type with Lambda with TypeLike {

    type This = LambdaPolyType
    val paramNames: List[u.TypeName] = paramTNames.map(encodeTypeName)

    override val productPrefix = "HKTypeLambda"

    registerCallback(this)

    val paramInfos: List[TypeBounds] = paramInfosOp()

    override val typeParams: List[Symbol] = paramNames.lazyZip(paramInfos).map {
      case (name, bounds) =>
        val argInfo = normaliseBounds(bounds)
        ctx.owner.newTypeParameter(name, noPosition, Deferred).setInfo(argInfo)
    }

    val resType: Type = lambdaResultType(resultTypeOp())

    validateThisLambda()

    def canonical: LambdaPolyType = mkLambdaPolyType(typeParams, resType)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[HKTypeLambda]
  }

  final class PolyTypeLambda(paramTNames: List[TypeName])(registerCallback: PolyTypeLambda => Unit,
    paramInfosOp: () => List[TypeBounds], resultTypeOp: () => Type)(implicit ctx: Context)
  extends Type with Lambda with TypeLike {

    type This = PolyType

    val paramNames: List[u.TypeName] = paramTNames.map(encodeTypeName)

    override val productPrefix = "PolyTypeLambda"

    registerCallback(this)

    val paramInfos: List[TypeBounds] = paramInfosOp()

    override val typeParams: List[Symbol] = paramNames.lazyZip(paramInfos).map {
      case (name, argInfo) => ctx.owner.newTypeParameter(name, noPosition, Deferred).setInfo(argInfo)
    }

    val resType: Type = resultTypeOp() // potentially need to flatten? (probably not, happens in typer in dotty)

    validateThisLambda()

    def canonical: PolyType = ui.polyType(typeParams, resType)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[PolyTypeLambda]
  }

}
