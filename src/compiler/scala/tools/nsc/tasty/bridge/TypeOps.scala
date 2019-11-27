package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyFlags.{TastyFlagSet, EmptyFlags}

trait TypeOps extends TastyKernel with FlagOps {
  import FlagSets._

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

  def TypeRef(tpe: Type, name: Name, isModule: Boolean = false): Type = {
    val symName = if (tpe.members.containsName(name)) name else name.encode
    val member  = tpe.member(symName)
    mkTypeRef(tpe, if (isModule) member.linkedClassOfClass else member, Nil) // TODO tasty: refactor
  }

  abstract class LambdaTypeCompanion[N <: Name, PInfo <: Type, LT <: LambdaType] {
    def apply(paramNames: List[N])(paramInfosExp: LT => List[PInfo], resultTypeExp: LT => Type): LT
  }

  object TypeParamLambda {
    def apply(typeParams: List[Symbol], ret: Type): LambdaType = new TypeParamLambda(typeParams, ret)
  }

  final class TypeParamLambda(override val typeParams: List[Symbol], val resType: Type) extends LambdaType {
    type ThisName = TypeName
    type PInfo    = TypeBounds

    val paramNames: List[TypeName]   = typeParams.map(_.name.toTypeName)
    val paramInfos: List[TypeBounds] = typeParams.map(_.tpe.bounds)

    validateThisLambda()

    override val productPrefix                = "TypeParamLambda"
    override def canEqual(that: Any): Boolean = that.isInstanceOf[TypeParamLambda]
  }

  abstract class LambdaType extends Type with Product {
    type ThisName <: Name
    type PInfo <: Type

    val paramNames: List[ThisName]
    val paramInfos: List[PInfo]
    val resType: Type

    private[this] var myParamRefs: List[TypeParamRef] = _

    final def paramRefs: List[TypeParamRef] = {
      if (myParamRefs `eq` null) myParamRefs = paramNames.indices.toList.map(i => new TypeParamRef(this, i))
      myParamRefs
    }

    override final def safeToString: String = {
      val args = paramNames.zip(paramInfos).map {
        case (name, info) => s"${name}$info"
      }.mkString("[", ", ", "]")
      s"$args =>> $resType"
    }

    def typeParams: List[Symbol] // deferred to final implementation

    final protected def validateThisLambda(): Unit = {
      assert(resType.isComplete, this)
      assert(paramNames.nonEmpty, this)
      assert(paramInfos.length == paramNames.length, this)
    }

    /**Best effort to transform this to an equivalent canonical representation in scalac.
     */
    final def canonicalForm: Type = {
      val resUpper = resType.upperBound
      val resLower = if (resType `eq` resType.bounds) resType.lowerBound else defn.NothingTpe
      if (resUpper.typeArgs.nonEmpty && resUpper.typeArgs == paramInfos) {
        val resUpperRef = resUpper.asInstanceOf[TypeRef]
        mkPolyType(
          typeParams,
          mkTypeBounds(
            resLower,
            mkExistentialType(
              typeParams,
              mkTypeRef(resUpperRef.pre, resUpperRef.sym, typeParams.map(_.tpe))
            )
          )
        )
      }
      else if (resUpper.typeArgs.isEmpty) {
        mkPolyType(typeParams, mkTypeBounds(resLower, resUpper))
      }
      else if (resUpper.typeArgs == paramRefs) {
        resUpper.typeConstructor
      }
      else {
        this
      }
    }

    final def productArity: Int = 2
    final def productElement(n: Int): Any = n match {
      case 0 => paramNames
      case 1 => resType
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }
    def canEqual(that: Any): Boolean = that.isInstanceOf[LambdaType]
    override final def equals(that: Any): Boolean = that match {
      case lambdaType: LambdaType =>
        (lambdaType.canEqual(this)
          && lambdaType.paramNames == paramNames
          && lambdaType.resType == resType)
      case _ => false
    }
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
        paramInfosExp: HKTypeLambda => List[TypeBounds], resultTypeExp: HKTypeLambda => Type): HKTypeLambda =
      new HKTypeLambda(paramNames)(paramInfosExp, resultTypeExp)
  }

  final class HKTypeLambda(val paramNames: List[TypeName])(
      paramInfosExp: HKTypeLambda => List[TypeBounds], resultTypeExp: HKTypeLambda => Type)
  extends LambdaType {
    type ThisName = TypeName
    type PInfo = TypeBounds

    private[this] var myTypeParams: List[Symbol] = _

    override val productPrefix       = "HKTypeLambda"
    val paramInfos: List[TypeBounds] = paramInfosExp(this)
    val resType: Type                = resultTypeExp(this)

    validateThisLambda()

    override def typeParams: List[Symbol] = {
      if (myTypeParams `eq` null) myTypeParams = paramNames.zip(paramInfos).map {
        case (name, info) => mkNewFreeTypeSymbol(name.toTypeName, Param | Deferred, name.toString).setInfo(info)
      }
      myTypeParams
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[HKTypeLambda]
  }
}
