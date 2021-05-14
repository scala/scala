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

import scala.tools.nsc.tasty.{TastyUniverse, SafeEq, TastyModes}, TastyModes._

import scala.tools.tasty.{TastyName, ErasedTypeRef, TastyFlags}, TastyFlags._

import scala.reflect.internal.Variance
import scala.util.chaining._

import scala.collection.mutable
import scala.reflect.internal.Flags

/**This layer adds factories that construct `scala.reflect` Types in the shapes that TASTy expects.
 * Additionally provides operations to select a type from a type, or a type from a type with an additional prefix,
 * using a `TastyName`.
 */
trait TypeOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  @inline final def mergeableParams(t: Type, u: Type): Boolean =
    t.typeParams.size == u.typeParams.size

  /** `*:` erases to either TupleXXL or Product */
  @inline final def genTupleIsUnsupported[T](name: String)(implicit ctx: Context): T = unsupportedError(s"generic tuple type $name in ${boundsString(ctx.owner)}")
  @inline final def bigFnIsUnsupported[T](tpeStr: String)(implicit ctx: Context): T = unsupportedError(s"function type with more than 22 parameters in ${boundsString(ctx.owner)}: $tpeStr")
  @inline final def ctxFnIsUnsupported[T](tpeStr: String)(implicit ctx: Context): T = unsupportedError(s"context function type in ${boundsString(ctx.owner)}: $tpeStr")
  @inline final def unionIsUnsupported[T](implicit ctx: Context): T = unsupportedError(s"union in ${boundsString(ctx.owner)}")
  @inline final def matchTypeIsUnsupported[T](implicit ctx: Context): T = unsupportedError(s"match type in ${boundsString(ctx.owner)}")
  @inline final def erasedRefinementIsUnsupported[T](implicit ctx: Context): T = unsupportedError(s"erased modifier in refinement of ${ctx.owner}")
  @inline final def polyFuncIsUnsupported[T](tpe: Type)(implicit ctx: Context): T = unsupportedError(s"polymorphic function type in ${boundsString(ctx.owner)}: $tpe")

  @inline final def isConstantType(tpe: Type): Boolean = tpe.isInstanceOf[u.ConstantType]

  @inline final def isTypeType(tpe: Type): Boolean = !((tpe `eq` u.ErrorType) || (tpe `eq` u.NoType))

  private object UnmergablePolyBounds {
    def unapply(tpe: u.TypeBounds): Boolean = tpe match {
      case u.TypeBounds(lo: u.PolyType, hi: u.PolyType) => !mergeableParams(lo,hi)
      case _                                            => false
    }
  }

  def lzyShow(tpe: Type): String = tpe match {
    case u.TypeRef(_, sym, args) => s"$sym${if (args.nonEmpty) args.map(lzyShow).mkString("[", ",","]") else ""}"
    case tpe                     => tpe.typeSymbolDirect.toString
  }

  def fnResult(fn: Type): Type = fn.dealiasWiden.finalResultType
  def tyconResult(tycon: Type, args: List[Type]): Type = tycon.resultType.substituteTypes(tycon.typeParams, args)

  /** return a type that can be used as a class type, e.g. in parents of another class, or as the type of new */
  def safeClassType(tpe: Type): Type = tpe match {
    case tpe: LambdaPolyType => tpe.toNested
    case tpe                 => tpe
  }

  def emptyTypeBounds: Type = u.TypeBounds.empty

  def intersectionParts(tpe: Type): List[Type] = tpe match {
    case tpe: u.RefinedType => tpe.parents
    case tpe                => tpe :: Nil
  }

  object defn {

    final val ChildAnnot: Symbol = u.definitions.ChildAnnotationClass
    final val RepeatedAnnot: Symbol = u.definitions.RepeatedAnnotationClass
    final val TargetNameAnnotationClass: Symbol = u.definitions.TargetNameAnnotationClass
    final val StaticMethodAnnotationClass: Symbol = u.definitions.StaticMethodAnnotationClass

    object PolyFunctionType {

      val PolyFunctionClass: Symbol = u.definitions.PolyFunctionClass

      def unapply(tpe: Type): Boolean = tpe match {
        case polyfnRef: u.TypeRef => polyfnRef.sym eq PolyFunctionClass
        case _                    => false
      }

    }

    final val NoType: Type = u.NoType

    /** Represents a symbol that has been initialised by TastyUnpickler, but can not be in a state of completion
     *  because its definition has not yet been seen.
     */
    object DefaultInfo extends TastyRepr {
      override def isTrivial: Boolean = true
      def originalFlagSet: TastyFlagSet = EmptyTastyFlags
    }

    private[bridge] def CopyInfo(underlying: u.TermSymbol, originalFlagSet: TastyFlagSet): TastyRepr =
      new CopyCompleter(underlying, originalFlagSet)

    def OpaqueTypeToBounds(tpe: Type): (Type, Type) = tpe match {
      case u.PolyType(tparams, tpe) =>
        val (bounds, alias) = OpaqueTypeToBounds(tpe)
        (u.PolyType(tparams, bounds), u.PolyType(tparams, alias))

      case tpe: OpaqueTypeBounds => (tpe, tpe.alias)

      case _ =>
        // An alias opaque type is defined as IDENTtpt with a simple type, so has no bounds
        (u.TypeBounds.empty, tpe)

    }
    def ByNameType(arg: Type): Type = u.definitions.byNameType(arg)
    def TypeBounds(lo: Type, hi: Type): Type = u.TypeBounds.apply(lo, hi)
    def SingleType(pre: Type, sym: Symbol): Type = u.singleType(pre, sym)
    def ExprType(res: Type): Type = u.NullaryMethodType(res)
    def InlineExprType(res: Type): Type = res match {
      case u.ConstantType(value) => u.NullaryMethodType(u.FoldableConstantType(value))
      case x                     => throw new MatchError(x)
    }
    def PolyType(params: List[Symbol], res: Type): Type = u.PolyType(params, res)
    def ClassInfoType(parents: List[Type], clazz: Symbol): Type = u.ClassInfoType(parents, clazz.rawInfo.decls, clazz.asType)
    def ClassInfoType(parents: List[Type], decls: List[Symbol], clazz: Symbol): Type = u.ClassInfoType(parents, u.newScopeWith(decls:_*), clazz.asType)
    def ThisType(sym: Symbol): Type = u.ThisType(sym)
    def ConstantType(c: Constant): Type = u.ConstantType(c)
    def IntersectionType(tps: Type*): Type = u.intersectionType(tps.toList)
    def IntersectionType(tps: List[Type]): Type = u.intersectionType(tps)

    def AnnotatedType(tpe: Type, annot: Tree): Type = tpe match {
      case u.AnnotatedType(annots, tpe) => u.AnnotatedType(annots :+ mkAnnotation(annot), tpe)
      case _                            => u.AnnotatedType(mkAnnotation(annot) :: Nil   , tpe)
    }

    def SuperType(thisTpe: Type, superTpe: Type): Type = u.SuperType(thisTpe, superTpe)
    def LambdaFromParams(typeParams: List[Symbol], ret: Type): Type = u.PolyType(typeParams, lambdaResultType(ret))
    def RecType(run: RecType => Type)(implicit ctx: Context): Type = new RecType(run).parent

    /** The method type corresponding to given parameters and result type */
    def DefDefType(typeParams: List[Symbol], valueParamss: List[List[Symbol]], resultType: Type): Type = {
      var tpe = valueParamss.foldRight(resultType)((ts, res) => u.MethodType(ts, res))
      if (valueParamss.isEmpty) tpe = u.NullaryMethodType(tpe)
      if (typeParams.nonEmpty)  tpe = u.PolyType(typeParams, tpe)
      tpe
    }

    def RefinedType(parent: Type, name: TastyName, refinedCls: Symbol, tpe: Type)(implicit ctx: Context): Type = {
      val decl = ctx.newRefinementSymbol(parent, refinedCls, name, tpe)
      parent match {
        case defn.PolyFunctionType() =>
          polyFuncIsUnsupported(tpe)
        case nested: u.RefinedType =>
          mkRefinedTypeWith(nested.parents, refinedCls, nested.decls.cloneScope.tap(_.enter(decl)))
        case _ =>
          mkRefinedTypeWith(parent :: Nil, refinedCls, u.newScopeWith(decl))
      }
    }

    def NormalisedBounds(tpe: Type, sym: Symbol)(implicit ctx: Context): Type = tpe match {
      case bounds @ UnmergablePolyBounds() =>
        unsupportedError(s"diverging higher kinded bounds: $sym$bounds")
      case tpe: u.TypeBounds => normaliseBounds(tpe)
      case tpe               => tpe
    }

    def AppliedType(tycon: Type, args: List[Type])(implicit ctx: Context): Type = {

      def formatFnType(arrow: String, arity: Int, args: List[Type]): String = {
        val len = args.length
        assert(len == arity + 1) // tasty should be type checked already
        val res = args.last
        val params = args.init
        val paramsBody = params.mkString(",")
        val argList = if (len == 2) paramsBody else s"($paramsBody)"
        s"$argList $arrow $res"
      }

      def typeRefUncurried(tycon: Type, args: List[Type]): Type = tycon match {
        case tycon: u.TypeRef if tycon.typeArgs.nonEmpty =>
          unsupportedError(s"curried type application $tycon[${args.mkString(",")}]")
        case ContextFunctionType(n) => ctxFnIsUnsupported(formatFnType("?=>", n, args))
        case FunctionXXLType(n)     => bigFnIsUnsupported(formatFnType("=>", n, args))
        case _ =>
          u.appliedType(tycon, args)
      }

      if (args.exists(tpe => tpe.isInstanceOf[u.TypeBounds] | tpe.isInstanceOf[LambdaPolyType])) {
        val syms = mutable.ListBuffer.empty[Symbol]
        def bindWildcards(tpe: Type) = tpe match {
          case tpe: u.TypeBounds   => ctx.newWildcard(tpe).tap(syms += _).pipe(_.ref)
          case tpe: LambdaPolyType => tpe.toNested
          case tpe                 => tpe
        }
        val args1 = args.map(bindWildcards)
        if (syms.isEmpty) typeRefUncurried(tycon, args1)
        else u.ExistentialType(syms.toList, typeRefUncurried(tycon, args1))
      }
      else {
        typeRefUncurried(tycon, args)
      }

    }
  }

  private[bridge] def mkRefinedTypeWith(parents: List[Type], clazz: Symbol, decls: u.Scope): Type =
    u.RefinedType.apply(parents, decls, clazz).tap(clazz.info = _)

  private def normaliseIfBounds(tpe: Type): Type = tpe match {
    case tpe: u.TypeBounds => normaliseBounds(tpe)
    case tpe               => tpe
  }

  private def normaliseBounds(bounds: u.TypeBounds): Type = {
    val u.TypeBounds(lo, hi) = bounds
    if (lo.isHigherKinded && hi.isHigherKinded) {
      if (mergeableParams(lo, hi)) {
        val nuLo = lo.resultType.upperBound.subst(lo.typeParams, hi.typeParams.map(_.ref))
        lo.typeParams.foreach { sym =>
          sym.owner.rawInfo.decls.unlink(sym)
          sym.owner.rawInfo.members.unlink(sym)
          sym.owner = noSymbol
        }
        u.PolyType(hi.typeParams, u.TypeBounds(nuLo, hi.resultType.upperBound))
      }
      else bounds match {
        case u.TypeBounds(lo: LambdaPolyType, hi: LambdaPolyType) => u.TypeBounds(lo.toNested,hi.toNested)
        case _                                                    => bounds
      }
    }
    else if (hi.isHigherKinded)
      u.PolyType(hi.typeParams, u.TypeBounds(lo.upperBound, hi.resultType.upperBound))
    else if (lo.isHigherKinded)
      u.PolyType(lo.typeParams, u.TypeBounds(lo.resultType.upperBound, hi.upperBound))
    else
      bounds
  }

  private[bridge] def sameErasure(sym: Symbol)(tpe: Type, ref: ErasedTypeRef) =
    NameErasure.sigName(tpe, sym) === ref

  /** This is a port from Dotty of transforming a Method type to an ErasedTypeRef
   */
  private[bridge] object NameErasure {

    def isRepeatedParam(self: Type): Boolean =
      self.typeSymbol eq u.definitions.RepeatedParamClass

    /** Translate a type of the form From[T] to either To[T] or To[? <: T] (if `wildcardArg` is set). Keep other types as they are.
     *  `from` and `to` must be static classes, both with one type parameter, and the same variance.
     *  Do the same for by name types => From[T] and => To[T]
     */
    def translateParameterized(self: Type)(from: u.ClassSymbol, to: u.ClassSymbol, wildcardArg: Boolean): Type = self match {
      case self @ u.NullaryMethodType(tp) =>
        u.NullaryMethodType(translateParameterized(tp)(from, to, wildcardArg = false))
      case _ =>
        if (self.typeSymbol.isSubClass(from)) {
          def elemType(tp: Type): Type = tp.dealiasWiden match {
            // case tp: AndOrType => tp.derivedAndOrType(elemType(tp.tp1), elemType(tp.tp2))
            case tp: u.RefinedType => u.intersectionType(tp.parents.map(elemType))
            case _ => tp.baseType(from).typeArgs.head
          }
          val arg = elemType(self)
          val arg1 = if (wildcardArg) u.TypeBounds.upper(arg) else arg
          to.ref(arg1 :: Nil)
        }
        else self
    }

    def translateFromRepeated(self: Type)(toArray: Boolean): Type = {
      val seqClass = if (toArray) u.definitions.ArrayClass else u.definitions.SeqClass
      if (isRepeatedParam(self))
        // We want `Array[? <: T]` because arrays aren't covariant until after
        // erasure. See `tests/pos/i5140`.
        translateParameterized(self)(u.definitions.RepeatedParamClass, seqClass, wildcardArg = toArray)
      else self
    }

    def sigName(tp: Type, sym: Symbol): ErasedTypeRef = {
      val normTp = translateFromRepeated(tp)(toArray = sym.isJavaDefined)
      erasedSigName(
        u.erasure.erasure(sym)(normTp)
      )
    }

    private def erasedSigName(erased: Type): ErasedTypeRef = erased match {
      case erased: u.ExistentialType => erasedSigName(erased.underlying)
      case erased: u.TypeRef =>
        import TastyName._
        if (!isSymbol(erased.sym))
          typeError(s"missing: ${erased.prefix}, ${erased.sym.name}")
        var dims = 0
        var clazzRef: Type = erased
        while (clazzRef.typeArgs.nonEmpty && clazzRef.typeSymbol.isSubClass(u.definitions.ArrayClass)) {
          dims += 1
          clazzRef = clazzRef.typeArgs.head
        }
        def unpeelName(acc: List[TastyName], tpe: Type): List[TastyName] = {
          def mkRef(sym: Symbol) = {
            val name = SimpleName(sym.name.toString)
            if (sym.isModuleClass && !sym.isPackageClass) ObjectName(name)
            else name
          }
          def rec(pre: Type) =
            (pre ne u.NoPrefix) && (pre ne u.NoType) && (pre.typeSymbol != u.rootMirror.RootClass)
          tpe match {
            case u.TypeRef(pre, sym, _) =>
              val ref = mkRef(sym)
              if (rec(pre)) unpeelName(ref :: acc, pre)
              else ref :: acc
            case tpe @ u.ThisType(sym) =>
              val ref = mkRef(sym)
              val pre = tpe.prefix
              if (rec(pre)) unpeelName(ref :: acc, pre)
              else ref :: acc
            case x => throw new MatchError(x)
          }
        }
        val name = (unpeelName(Nil, clazzRef): @unchecked) match {
          case single :: Nil => single
          case base :: rest  => rest.foldLeft(base)((acc, n) => n match {
            case ObjectName(base) => ObjectName(QualifiedName(acc, PathSep, base.asSimpleName))
            case name => QualifiedName(acc, PathSep, name.asSimpleName)
          })
        }
        ErasedTypeRef(name.toTypeName, dims)
      case u.ErrorType =>
        ErasedTypeRef(tpnme.ErrorType, 0)
      case x => throw new MatchError(x)
    }

  }

  /** A synthetic type `scala.&` which accepts two type arguments, representing an intersection type
   * @see https://github.com/lampepfl/dotty/issues/7688
   */
  case object AndTpe extends Type

  case class ContextFunctionType(arity: Int) extends Type {
    assert(arity > 0)
  }

  case class FunctionXXLType(arity: Int) extends Type {
    assert(arity > 22)
  }

  private val SyntheticScala3Type =
    raw"^(?:&|\||AnyKind|(?:Context)?Function\d+|\*:|Tuple|Matchable)$$".r

  def selectType(name: TastyName.TypeName, prefix: Type)(implicit ctx: Context): Type = selectType(name, prefix, prefix)
  def selectType(name: TastyName.TypeName, prefix: Type, space: Type)(implicit ctx: Context): Type = {
    import scala.tools.tasty.TastyName._

    def lookupType = namedMemberOfTypeWithPrefix(prefix, space, name)

    // we escape some types in the scala package especially
    if (prefix.typeSymbol === u.definitions.ScalaPackage) {
      name match {
        case TypeName(SimpleName(raw @ SyntheticScala3Type())) => raw match {
          case tpnme.And                                              => AndTpe
          case tpnme.Or                                               => unionIsUnsupported
          case tpnme.ContextFunctionN(n) if (n.toInt > 0)             => ContextFunctionType(n.toInt)
          case tpnme.FunctionN(n)        if (n.toInt > 22)            => FunctionXXLType(n.toInt)
          case tpnme.TupleCons                                        => genTupleIsUnsupported("scala.*:")
          case tpnme.Tuple               if !ctx.mode.is(ReadParents) => genTupleIsUnsupported("scala.Tuple")
          case tpnme.AnyKind                                          => u.definitions.AnyTpe
          case tpnme.Matchable                                        => u.definitions.AnyTpe
          case _                                                      => lookupType
        }

        case _ => lookupType
      }
    }
    else {
      lookupType
    }
  }

  def selectTerm(name: TastyName, prefix: Type)(implicit ctx: Context): Type = selectTerm(name, prefix, prefix)
  def selectTerm(name: TastyName, prefix: Type, space: Type)(implicit ctx: Context): Type =
    namedMemberOfTypeWithPrefix(prefix, space, name.toTermName)

  def singletonLike(tpe: Type): Symbol = tpe match {
    case u.SingleType(_, sym) => sym
    case u.TypeRef(_,sym,_)   => sym
    case x                    => throw new MatchError(x)
  }

  private[TypeOps] val NoSymbolFn = (_: Context) => u.NoSymbol

  sealed abstract trait TastyRepr extends u.Type {
    def originalFlagSet: TastyFlagSet
    final def tastyOnlyFlags: TastyFlagSet = originalFlagSet & FlagSets.TastyOnlyFlags
  }

  abstract class TastyCompleter(isClass: Boolean, final val originalFlagSet: TastyFlagSet)(implicit
      capturedCtx: Context) extends u.LazyType with TastyRepr with u.FlagAgnosticCompleter {

    override final val decls: u.Scope = if (isClass) u.newScope else u.EmptyScope

    override final def load(sym: Symbol): Unit =
      complete(sym)

    override final def complete(sym: Symbol): Unit =
      // we do have to capture Context here as complete is triggered outside of our control
      // TODO [tasty]: perhaps Context can be redesigned so it can be reconstructed from a lightweight representation.
      computeInfo(sym)(capturedCtx)

    /**Compute and set the info for the symbol in the given Context
     */
    def computeInfo(sym: Symbol)(implicit ctx: Context): Unit
  }

  private[TypeOps] class CopyCompleter(underlying: u.TermSymbol, final val originalFlagSet: TastyFlagSet)
      extends u.LazyType with TastyRepr with u.FlagAgnosticCompleter {
    override final def complete(sym: Symbol): Unit = {
      underlying.ensureCompleted()
      sym.info = underlying.tpe
      underlying.attachments.all.foreach(sym.updateAttachment(_))
    }
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
    else {
      u.singleType(prefix, sym)
    }
  }

  def normaliseConstructorRef(ctor: Symbol): Type = {
    var tpe = ctor.tpe
    val tParams = ctor.owner.typeParams
    if (tParams.nonEmpty) tpe = u.PolyType(tParams, tpe)
    tpe
  }

  def namedMemberOfPrefix(pre: Type, name: TastyName)(implicit ctx: Context): Type =
    namedMemberOfTypeWithPrefix(pre, pre, name)

  def namedMemberOfTypeWithPrefix(pre: Type, space: Type, tname: TastyName)(implicit ctx: Context): Type = {
    prefixedRef(pre, namedMemberOfType(space, tname))
  }

  def lambdaResultType(resType: Type): Type = resType match {
    case res: LambdaPolyType => res.toNested
    case res                 => res
  }

  abstract class LambdaTypeCompanion[N <: TastyName] {
    def factory(params: List[N])(registerCallback: Type => Unit, paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context): LambdaType

    final def apply(params: List[N])(registerCallback: Type => Unit, paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context): Type =
      factory(params)(registerCallback, paramInfosOp, resultTypeOp).canonical
  }

  final class LambdaPolyType(typeParams: List[Symbol], val resType: Type) extends u.PolyType(typeParams, LambdaPolyType.addLower(resType)) {
    def toNested: u.PolyType = resType match {
      case _: u.TypeBounds => this
      case _               => u.PolyType(typeParams, resType)
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
    private def addLower(tpe: Type): u.TypeBounds = tpe match {
      case tpe: u.TypeBounds => tpe
      case tpe               => u.TypeBounds.upper(tpe)
    }
  }

  private[bridge] final class OpaqueTypeBounds(lo: Type, hi: Type, val alias: Type) extends u.TypeBounds(lo, hi)

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
  }

  private[TypeOps] trait TermLike { self: Type with Lambda =>
    type ThisTName = TastyName
    type ThisName  = u.TermName
    type PInfo     = Type
  }

  private[TypeOps] trait Lambda extends Product with Serializable { self: Type =>
    type ThisTName <: TastyName
    type ThisName <: u.Name
    type This <: Type

    val paramNames: List[ThisName]
    val paramInfos: List[Type]
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
        paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context): LambdaType =
      new HKTypeLambda(params)(registerCallback, paramInfosOp, resultTypeOp)
  }

  object PolyType extends TypeLambdaCompanion {
    def factory(params: List[TastyName.TypeName])(registerCallback: Type => Unit,
         paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context): LambdaType =
      new PolyTypeLambda(params)(registerCallback, paramInfosOp, resultTypeOp)
  }

  final class MethodTypeCompanion(defaultFlags: TastyFlagSet) extends TermLambdaCompanion { self =>
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
    val recThis: Type   = u.ThisType(refinementClass)
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

  def methodTypeCompanion(initialFlags: TastyFlagSet): MethodTypeCompanion = new MethodTypeCompanion(initialFlags)

  abstract class TermLambdaCompanion
    extends LambdaTypeCompanion[TastyName]

  abstract class TypeLambdaCompanion
    extends LambdaTypeCompanion[TastyName.TypeName]

  private[TypeOps] final class MethodTermLambda(paramTNames: List[TastyName], defaultFlags: TastyFlagSet)(registerCallback: MethodTermLambda => Unit,
    paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context)
  extends Type with Lambda with TermLike { methodLambda =>
    type This = u.MethodType

    val paramNames: List[u.TermName] = paramTNames.map(encodeTermName)

    override val productPrefix = "MethodTermLambda"

    registerCallback(this)

    val paramInfos: List[Type] = paramInfosOp()

    override val params: List[Symbol] = paramNames.lazyZip(paramInfos).map {
      case (name, argInfo) =>
        ctx.owner.newValueParameter(name, u.NoPosition, newSymbolFlagSet(defaultFlags)).setInfo(argInfo)
    }

    val resType: Type = resultTypeOp()

    validateThisLambda()

    def canonical: u.MethodType = u.MethodType(params, resType)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[MethodTermLambda]
  }

  private[TypeOps] final class HKTypeLambda(paramTNames: List[TastyName.TypeName])(registerCallback: HKTypeLambda => Unit,
    paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context)
  extends Type with Lambda with TypeLike {

    type This = LambdaPolyType
    val paramNames: List[u.TypeName] = paramTNames.map(encodeTypeName)

    override val productPrefix = "HKTypeLambda"

    registerCallback(this)

    val paramInfos: List[Type] = paramInfosOp()

    override val typeParams: List[Symbol] = paramNames.lazyZip(paramInfos).map {
      case (name, bounds) =>
        val argInfo = normaliseIfBounds(bounds)
        ctx.owner.newTypeParameter(name, u.NoPosition, FlagSets.Creation.BoundedType).setInfo(argInfo)
    }

    val resType: Type = lambdaResultType(resultTypeOp())

    validateThisLambda()

    def canonical: LambdaPolyType = new LambdaPolyType(typeParams, resType)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[HKTypeLambda]
  }

  private[TypeOps] final class PolyTypeLambda(paramTNames: List[TastyName.TypeName])(registerCallback: PolyTypeLambda => Unit,
    paramInfosOp: () => List[Type], resultTypeOp: () => Type)(implicit ctx: Context)
  extends Type with Lambda with TypeLike {

    type This = u.PolyType

    val paramNames: List[u.TypeName] = paramTNames.map(encodeTypeName)

    override val productPrefix = "PolyTypeLambda"

    registerCallback(this)

    val paramInfos: List[Type] = paramInfosOp()

    override val typeParams: List[Symbol] = paramNames.lazyZip(paramInfos).map {
      case (name, argInfo) =>
        ctx.owner.newTypeParameter(name, u.NoPosition, FlagSets.Creation.BoundedType).setInfo(argInfo)
    }

    val resType: Type = resultTypeOp() // potentially need to flatten? (probably not, happens in typer in dotty)

    validateThisLambda()

    def canonical: u.PolyType = u.PolyType(typeParams, resType)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[PolyTypeLambda]
  }

}
