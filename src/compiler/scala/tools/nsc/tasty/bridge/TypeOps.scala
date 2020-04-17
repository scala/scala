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

import scala.tools.nsc.tasty.{TastyUniverse, SafeEq, TastyFlags}, TastyFlags._

import scala.tools.tasty.{TastyName, ErasedTypeRef}

import scala.reflect.internal.Variance
import scala.util.chaining._

import scala.collection.mutable
import scala.reflect.internal.Flags

trait TypeOps { self: TastyUniverse =>
  import self.{symbolTable => u}, u.{internal => ui}

  @inline final def mergeableParams(t: Type, u: Type): Boolean =
    t.typeParams.size == u.typeParams.size

  @inline final def unionIsUnsupported[T](implicit ctx: Context): T = unsupportedError(s"union in bounds of ${ctx.owner}")
  @inline final def matchTypeIsUnsupported[T](implicit ctx: Context): T = unsupportedError(s"match type in bounds of ${ctx.owner}")
  @inline final def erasedRefinementIsUnsupported[T](implicit ctx: Context): T = unsupportedError(s"erased modifier in refinement of ${ctx.owner}")

  @inline final def isConstantType(tpe: Type): Boolean = tpe.isInstanceOf[u.ConstantType]

  @inline final def isTypeType(tpe: Type): Boolean = !((tpe `eq` u.ErrorType) || (tpe `eq` u.NoType))

  private object UnmergablePolyBounds {
    def unapply(tpe: TypeBounds): Boolean = tpe match {
      case u.TypeBounds(lo: u.PolyType, hi: u.PolyType) => !mergeableParams(lo,hi)
      case _                                            => false
    }
  }

  def fnResult(fn: Type): Type = fn.dealiasWiden.finalResultType
  def tyconResult(tycon: Type, args: List[Type]): Type = tycon.resultType.substituteTypes(tycon.typeParams, args)

  def emptyTypeBounds: Type = u.TypeBounds.empty

  object defn {
    final val NoType: Type = u.NoType
    def ByNameType(arg: Type): Type = u.definitions.byNameType(arg)
    def TypeBounds(lo: Type, hi: Type): Type = u.TypeBounds.apply(lo, hi)
    def SingleType(pre: Type, sym: Symbol): Type = u.singleType(pre, sym)
    def ExprType(res: Type): Type = ui.nullaryMethodType(res)
    def PolyType(params: List[Symbol], res: Type): Type = ui.polyType(params, res)
    def ClassInfoType(parents: List[Type], decls: Scope, sym: Symbol): Type = ui.classInfoType(parents, decls, sym)
    def ThisType(sym: Symbol): Type = ui.thisType(sym)
    def ConstantType(c: Constant): Type = ui.constantType(c)
    def IntersectionType(tps: Type*): Type = ui.intersectionType(tps.toList)
    def IntersectionType(tps: List[Type]): Type = ui.intersectionType(tps)
    def AnnotatedType(tpe: Type, annot: Annotation): Type = u.AnnotatedType(annot :: Nil, tpe)
    def SuperType(thisTpe: Type, superTpe: Type): Type = u.SuperType(thisTpe, superTpe)
    def LambdaFromParams(typeParams: List[Symbol], ret: Type): Type = ui.polyType(typeParams, lambdaResultType(ret))
    def RecType(run: RecType => Type)(implicit ctx: Context): Type = new RecType(run).parent

    /** The method type corresponding to given parameters and result type */
    def DefDefType(typeParams: List[Symbol], valueParamss: List[List[Symbol]], resultType: Type): Type = {
      var tpe = valueParamss.foldRight(resultType)((ts, res) => ui.methodType(ts, res))
      if (valueParamss.isEmpty) tpe = ui.nullaryMethodType(tpe)
      if (typeParams.nonEmpty)  tpe = ui.polyType(typeParams, tpe)
      tpe
    }

    def RefinedType(parent: Type, name: TastyName, refinedCls: Symbol, tpe: Type)(implicit ctx: Context): Type = {
      val decl = ctx.newRefinementSymbol(parent, refinedCls, name, tpe)
      parent match {
        case nested: u.RefinedType =>
          mkRefinedTypeWith(nested.parents, refinedCls, nested.decls.cloneScope.tap(_.enter(decl)))
        case _ =>
          mkRefinedTypeWith(parent :: Nil, refinedCls, ctx.mkScope(decl))
      }
    }

    def NormalisedBounds(tpe: Type, sym: Symbol)(implicit ctx: Context): Type = tpe match {
      case bounds @ UnmergablePolyBounds() =>
        unsupportedError(s"diverging higher kinded bounds: $sym$bounds")
      case tpe: TypeBounds => normaliseBounds(tpe)
      case tpe             => tpe
    }

    def AppliedType(tycon: Type, args: List[Type])(implicit ctx: Context): Type = {

      def typeRefUncurried(tycon: Type, args: List[Type]): Type = tycon match {
        case tycon: u.TypeRef if tycon.typeArgs.nonEmpty =>
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
  }

  private[bridge] def mkRefinedTypeWith(parents: List[Type], clazz: Symbol, decls: Scope): Type =
    u.RefinedType.apply(parents, decls, clazz).tap(clazz.info = _)

  private def normaliseBounds(bounds: TypeBounds): Type = {
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

  private[bridge] def resolveErasedTypeRef(ref: ErasedTypeRef)(implicit ctx: Context): Type = {
    import TastyName._

    val sym = ref.qualifiedName match {
      case TypeName(module: ModuleName) => ctx.requiredModule(module)
      case clazz                        => ctx.requiredClass(clazz)
    }

    (0 until ref.arrayDims).foldLeft(sym.tpe.erasure)((acc, _) => u.definitions.arrayType(acc))
  }

  /** A type which accepts two type arguments, representing an intersection type
   * @see https://github.com/lampepfl/dotty/issues/7688
   */
  case object AndType extends Type

  def selectType(name: TastyName.TypeName, prefix: Type)(implicit ctx: Context): Type = selectType(name, prefix, prefix)
  def selectType(name: TastyName.TypeName, prefix: Type, space: Type)(implicit ctx: Context): Type = {
    if (prefix.typeSymbol === u.definitions.ScalaPackage && ( name === tpnme.And || name === tpnme.Or ) ) {
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
    private[this] var myTastyFlagSet: TastyFlagSet = EmptyTastyFlags

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
        case tp: u.ThisType if tp.sym.isRefinementClass => sym.preciseRef(prefix)
        case _:u.SingleType | _:u.RefinedType           => sym.preciseRef(prefix)
        case _                                          => sym.ref
      }
    }
    else if (sym.isConstructor) {
      normaliseConstructorRef(sym)
    }
    else if (sym.is(Static)) {
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

  abstract class LambdaTypeCompanion[N <: TastyName, PInfo <: Type] {
    def factory(params: List[N])(registerCallback: Type => Unit, paramInfosOp: () => List[PInfo], resultTypeOp: () => Type)(implicit ctx: Context): LambdaType

    final def apply(params: List[N])(registerCallback: Type => Unit, paramInfosOp: () => List[PInfo], resultTypeOp: () => Type)(implicit ctx: Context): Type =
      factory(params)(registerCallback, paramInfosOp, resultTypeOp).canonical
  }

  final class LambdaPolyType(typeParams: List[Symbol], resType: Type) extends u.PolyType(typeParams, LambdaPolyType.addLower(resType)) {
    def toNested: u.PolyType = resType match {
      case _: TypeBounds => this
      case _             => ui.polyType(typeParams, resType)
    }
    def withVariances(variances: List[Variance]): this.type = {
      typeParams.lazyZip(variances).foreach { (sym, variance) => // TODO [tasty]: should this be cloned instead?
        variance match {
          case Variance.Covariant => sym.flags |= Flags.COVARIANT
          case Variance.Contravariant => sym.flags |= Flags.CONTRAVARIANT
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

  private[TypeOps] type LambdaType = Type with Lambda
  private[TypeOps] type TypeLambda = LambdaType with TypeLike
  private[TypeOps] type TermLambda = LambdaType with TermLike

  private[TypeOps] trait TypeLike { self: Type with Lambda =>
    type ThisTName = TastyName.TypeName
    type ThisName  = u.TypeName
    type PInfo     = TypeBounds
  }

  private[TypeOps] trait TermLike { self: Type with Lambda =>
    type ThisTName = TastyName
    type ThisName  = u.TermName
    type PInfo     = Type
  }

  private[TypeOps] trait Lambda extends Product with Serializable { self: Type =>
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

  object HKTypeLambda extends TypeLambdaCompanion {
    def factory(params: List[TastyName.TypeName])(registerCallback: Type => Unit,
        paramInfosOp: () => List[TypeBounds], resultTypeOp: () => Type)(implicit ctx: Context): LambdaType =
      new HKTypeLambda(params)(registerCallback, paramInfosOp, resultTypeOp)
  }

  object PolyType extends TypeLambdaCompanion {
    def factory(params: List[TastyName.TypeName])(registerCallback: Type => Unit,
         paramInfosOp: () => List[TypeBounds], resultTypeOp: () => Type)(implicit ctx: Context): LambdaType =
      new PolyTypeLambda(params)(registerCallback, paramInfosOp, resultTypeOp)
  }

  abstract class MethodTypeCompanion(defaultFlags: TastyFlagSet) extends TermLambdaCompanion { self =>
    def factory(params: List[TastyName])(registerCallback: Type => Unit,
        paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context): LambdaType =
      new MethodTermLambda(params, defaultFlags)(registerCallback, paramInfosOp, resultTypeOp)
  }

  def recThis(tpe: Type): Type = tpe.asInstanceOf[RecType].recThis
  def symOfTypeRef(tpe: Type): Symbol = tpe.asInstanceOf[u.TypeRef].sym

  private[TypeOps] final class RecType(run: RecType => Type)(implicit ctx: Context) extends Type with Product {

    override val productPrefix = "RecType"
    override val productArity = 2

    val refinementClass = ctx.newRefinementClassSymbol
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

  object MethodType extends MethodTypeCompanion(EmptyTastyFlags)
  object ImplicitMethodType extends MethodTypeCompanion(Implicit)

  abstract class TermLambdaCompanion
    extends LambdaTypeCompanion[TastyName, Type]

  abstract class TypeLambdaCompanion
    extends LambdaTypeCompanion[TastyName.TypeName, TypeBounds]

  private[TypeOps] final class MethodTermLambda(paramTNames: List[TastyName], defaultFlags: TastyFlagSet)(registerCallback: MethodTermLambda => Unit,
    paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context)
  extends Type with Lambda with TermLike { methodLambda =>
    type This = u.MethodType

    val paramNames: List[u.TermName] = paramTNames.map(encodeTermName)

    override val productPrefix = "MethodTermLambda"

    registerCallback(this)

    val paramInfos: List[Type] = paramInfosOp()

    override val params: List[Symbol] = paramNames.lazyZip(paramInfos).map {
      case (name, argInfo) => ctx.owner.newValueParameter(name, u.NoPosition, encodeFlagSet(defaultFlags)).setInfo(argInfo)
    }

    val resType: Type = resultTypeOp()

    validateThisLambda()

    def canonical: u.MethodType = ui.methodType(params, resType)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[MethodTermLambda]
  }

  private[TypeOps] final class HKTypeLambda(paramTNames: List[TastyName.TypeName])(registerCallback: HKTypeLambda => Unit,
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
        ctx.owner.newTypeParameter(name, u.NoPosition, u.Flag.DEFERRED).setInfo(argInfo)
    }

    val resType: Type = lambdaResultType(resultTypeOp())

    validateThisLambda()

    def canonical: LambdaPolyType = new LambdaPolyType(typeParams, resType)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[HKTypeLambda]
  }

  private[TypeOps] final class PolyTypeLambda(paramTNames: List[TastyName.TypeName])(registerCallback: PolyTypeLambda => Unit,
    paramInfosOp: () => List[TypeBounds], resultTypeOp: () => Type)(implicit ctx: Context)
  extends Type with Lambda with TypeLike {

    type This = u.PolyType

    val paramNames: List[u.TypeName] = paramTNames.map(encodeTypeName)

    override val productPrefix = "PolyTypeLambda"

    registerCallback(this)

    val paramInfos: List[TypeBounds] = paramInfosOp()

    override val typeParams: List[Symbol] = paramNames.lazyZip(paramInfos).map {
      case (name, argInfo) => ctx.owner.newTypeParameter(name, u.NoPosition, u.Flag.DEFERRED).setInfo(argInfo)
    }

    val resType: Type = resultTypeOp() // potentially need to flatten? (probably not, happens in typer in dotty)

    validateThisLambda()

    def canonical: u.PolyType = ui.polyType(typeParams, resType)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[PolyTypeLambda]
  }

}
