/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.{TastyUniverse, SafeEq, TastyModes, ForceKinds}, TastyModes._, ForceKinds._

import scala.tools.tasty.{TastyName, ErasedTypeRef, TastyFlags}, TastyFlags._

import scala.reflect.internal.Variance
import scala.util.chaining._

import scala.collection.mutable
import scala.collection.immutable.ArraySeq

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
  @inline final def fnIsUnsupported[T](kind: String => String, tpeStr: String)(implicit ctx: Context): T = unsupportedError(s"${kind("function type")} in ${boundsString(ctx.owner)}: $tpeStr")
  @inline final def bigFnIsUnsupported[T](tpeStr: String)(implicit ctx: Context): T = fnIsUnsupported(ft => s"$ft with more than 22 parameters", tpeStr)
  @inline final def ctxFnIsUnsupported[T](tpeStr: String)(implicit ctx: Context): T = fnIsUnsupported(ft => s"context $ft", tpeStr)
  @inline final def erasedFnIsUnsupported[T](tpeStr: String)(implicit ctx: Context): T = fnIsUnsupported(ft => s"erased $ft", tpeStr)
  @inline final def erasedCtxFnIsUnsupported[T](tpeStr: String)(implicit ctx: Context): T = fnIsUnsupported(ft => s"erased context $ft", tpeStr)
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

  def lzyShow(tpe: Type): String = {
    val sym = symOfType(tpe)
    if (isSymbol(sym)) {
      val args = tpe.typeArgs
      s"${sym.fullName}${if (args.nonEmpty) args.map(lzyShow).mkString("[", ",", "]") else ""}"
    }
    else {
      s"${tpe.typeSymbolDirect.fullName}"
    }
  }

  def showType(tpe: Type, wrap: Boolean = true)(implicit ctx: Context): String = {
    def prefixed(prefix: String)(op: => String) = {
      val raw = op
      if (wrap) s"""$prefix"$raw""""
      else raw
    }
    def parameterised(tparams: List[Symbol], prefix: String)(f: String => String) = prefixed(prefix) {
      f(if (tparams.isEmpty) "" else tparams.map(p => s"${p.name}").mkString("[", ", ", "]"))
    }
    def cls(tparams: List[Symbol], tpe: u.ClassInfoType) = parameterised(tparams, "cls") { paramStr =>
      s"$paramStr${tpe.typeSymbol.fullName}$paramStr"
    }
    def meth(tparams: List[Symbol], tpe: u.MethodType) = parameterised(tparams, "meth") { paramStr =>
      s"$paramStr$tpe"
    }
    def preStr(pre: Type): String = {
      val preSym = symOfType(pre)
      val thisStr = {
        if (pre.isInstanceOf[u.ThisType] && !pre.typeSymbol.isPackageClass && !pre.typeSymbol.isModuleClass)
          ".this"
        else
          ""
      }
      if (isSymbol(preSym)) s"${preSym.fullName}$thisStr." else ""
    }
    tpe match {
      case tpe: u.ClassInfoType                      => cls(Nil, tpe)
      case u.PolyType(tparams, tpe: u.ClassInfoType) => cls(tparams, tpe)
      case u.PolyType(tparams, tpe: u.MethodType)    => meth(tparams, tpe)
      case tpe: u.MethodType                         => meth(Nil, tpe)
      case tpe: u.ThisType                           => prefixed("path") { s"${tpe.sym.fullName}.this" }

      case tpe: u.SingleType =>
        prefixed("path") {
          if (tpe.sym.isModule) tpe.sym.fullName + ".type"
          else s"${preStr(tpe.pre)}${tpe.sym.name}.type"
        }

      case tpe: u.TypeRef =>
        if (tpe.sym.is(Object)) prefixed("path") {
          s"${tpe.sym.fullName}.type"
        }
        else prefixed("tpelazy") {
          val pre = preStr(tpe.pre)
          val argsStrs = tpe.args.map(showType(_, wrap = false))
          val argsStr = if (argsStrs.nonEmpty) argsStrs.mkString("[", ", ", "]") else ""
          s"$pre${tpe.sym.name}$argsStr"
        }

      case tpe: u.TypeBounds => prefixed("tpebounds") { s"$tpe"}

      case tpe => prefixed("tpe") { s"$tpe" }
    }
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
    final val ExperimentalAnnotationClass: Symbol = u.definitions.ExperimentalAnnotationClass
    final val AnnotationDefaultClass: Symbol = u.definitions.AnnotationDefaultClass
    final val JavaAnnotationClass: Symbol = u.definitions.JavaAnnotationClass

    object PolyFunctionType {

      val PolyFunctionClass: Symbol = u.definitions.PolyFunctionClass

      def unapply(tpe: Type): Boolean = tpe match {
        case polyfnRef: u.TypeRef => polyfnRef.sym eq PolyFunctionClass
        case _                    => false
      }

    }

    final val NoType: Type = u.NoType
    final val NoPrefix: Type = u.NoPrefix

    final val ObjectTpe: Type = u.definitions.ObjectTpe
    final val ObjectTpeJava: Type = u.definitions.ObjectTpeJava

    def adjustParent(tp: Type)(implicit ctx: Context): Type = {
      val tpe = tp.dealias
      if (ctx.isJava && (tpe eq ObjectTpeJava)) ObjectTpe
      else if (tpe.typeSymbolDirect === u.definitions.ObjectClass) u.definitions.AnyRefTpe
      else tpe
    }

    /** Represents a symbol that has been initialised by TastyUnpickler, but can not be in a state of completion
     *  because its definition has not yet been seen.
     */
    object DefaultInfo extends TastyRepr {
      override def isTrivial: Boolean = true
      def tflags: TastyFlagSet = EmptyTastyFlags
    }

    private[bridge] def CopyInfo(underlying: u.TermSymbol, tflags: TastyFlagSet)(implicit ctx: Context): TastyRepr =
      new CopyCompleter(underlying, tflags)

    private[bridge] def SingletonEnumClassInfo(
        enumValue: u.TermSymbol,
        originalFlagSet: TastyFlagSet
    )(implicit ctx: Context): TastyRepr =
      new SingletonEnumModuleClassCompleter(enumValue, originalFlagSet)

    private[bridge] def LocalSealedChildProxyInfo(parent: Symbol, tflags: TastyFlagSet)(implicit ctx: Context): Type =
      new LocalSealedChildProxyCompleter(parent, tflags)

    private[bridge] def LambdaParamInfo(
        tflags: TastyFlagSet,
        idx: Int,
        infoDb: Int => Type
    )(implicit ctx: Context): Type =
      new LambdaParamCompleter(tflags, idx, infoDb)

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
    def InitialTypeInfo: Type = u.TypeBounds.empty
    def SingleType(pre: Type, sym: Symbol): Type = u.singleType(pre, sym)
    def ExprType(res: Type): Type = u.NullaryMethodType(res)
    def InlineExprType(res: Type): Type = res match {
      case u.ConstantType(value) => u.NullaryMethodType(u.FoldableConstantType(value))
      case x                     => throw new MatchError(x)
    }
    def PolyType(params: List[Symbol], res: Type): Type = u.PolyType(params, res)
    def ClassInfoType(parents: List[Type], clazz: Symbol): Type = u.ClassInfoType(parents, clazz.rawInfo.decls, clazz.asType)
    def ClassInfoType(parents: List[Type], decls: List[Symbol], clazz: Symbol): Type = u.ClassInfoType(parents, u.newScopeWith(decls:_*), clazz.asType)
    def ThisType(tpe: Type): Type = u.ThisType(symOfType(tpe))
    def ConstantType(c: Constant): Type = u.ConstantType(c)
    def IntersectionType(tps: Type*): Type = u.intersectionType(tps.toList)
    def IntersectionType(tps: List[Type]): Type = u.intersectionType(tps)

    def AnnotatedType(tpe: Type, annot: Tree)(implicit ctx: Context): Type = tpe match {
      case u.AnnotatedType(annots, tpe) => u.AnnotatedType(annots :+ mkAnnotation(annot, tpe), tpe)
      case _                            => u.AnnotatedType(mkAnnotation(annot, tpe) :: Nil   , tpe)
    }

    def SuperType(thisTpe: Type, superTpe: Type): Type = u.SuperType(thisTpe, superTpe)
    def LambdaFromParams(typeParams: List[Symbol], ret: Type): Type = u.PolyType(typeParams, lambdaResultType(ret))
    def RecType(run: RecType => Type)(implicit ctx: Context): Type = new RecType(run).parent
    def RecThis(tpe: Type): Type = tpe.asInstanceOf[RecType].recThis

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

      def formatFnType(arrow: String, isErased: Boolean, arity: Int, args: List[Type]): String = {
        val len = args.length
        assert(len == arity + 1) // tasty should be type checked already
        val res = args.last
        val params = args.init
        val paramsBody = {
          val body = params.mkString(",")
          if (isErased) s"erased $body" else body
        }
        val argList = if (len == 2) paramsBody else s"($paramsBody)"
        s"$argList $arrow $res"
      }

      def typeRefUncurried(tycon: Type, args: List[Type]): Type = tycon match {
        case tycon: u.TypeRef if tycon.typeArgs.nonEmpty =>
          unsupportedError(s"curried type application $tycon[${args.mkString(",")}]")
        case ContextFunctionType(n) => ctxFnIsUnsupported(formatFnType("?=>", isErased = false, n, args))
        case ErasedContextFunctionType(n) => erasedCtxFnIsUnsupported(formatFnType("?=>", isErased = true, n, args))
        case ErasedFunctionType(n) => erasedFnIsUnsupported(formatFnType("=>", isErased = true, n, args))
        case FunctionXXLType(n)     => bigFnIsUnsupported(formatFnType("=>", isErased = false, n, args))
        case _ =>
          if (ctx.isJava && tycon.typeSymbol === u.definitions.ArrayClass) {
            val arg0 = args.head
            val arg1 =
              arg0 match {
                case arg0: u.RefinedType if arg0.parents.exists(_ eq ObjectTpeJava) =>
                  // TODO [tasty]: in theory we could add more Modes to context to
                  // detect this situation and not perform the substitution ahead of time,
                  // however this does not work with SHAREDtype which caches.
                  val parents1 = arg0.parents.map(tpe =>
                    if (tpe eq ObjectTpeJava) ObjectTpe else tpe
                  )
                  IntersectionType(parents1)
                case _ =>
                  arg0
              }

            val args1 = if (arg1 eq arg0) args else arg1 :: Nil
            u.appliedType(tycon, args1)
          } else {
            u.appliedType(tycon, args)
          }
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

    def ParamRef(binder: Type, idx: Int): Type =
      binder.asInstanceOf[LambdaType].lambdaParams(idx).ref

    def NamedType(prefix: Type, sym: Symbol)(implicit ctx: Context): Type = {
      if (ctx.isJava && sym.isClass && sym.isJavaDefined) {
        def processInner(tp: Type): Type = tp match {
          case u.TypeRef(pre, sym, args) if !sym.isStatic => u.typeRef(processInner(pre.widen), sym, args)
          case _ => tp
        }
        prefix match {
          case _: u.TypeRef => processInner(u.typeRef(prefix, sym, Nil)) // e.g. prefix is `Foo[Int]`
          case _            => processInner(sym.tpeHK) // ignore prefix otherwise
        }
      }
      else if (sym.isType) {
        prefix match {
          case _: u.ThisType if !sym.isTypeParameter => u.typeRef(prefix, sym, Nil)
          case _:u.SingleType | _:u.RefinedType      => u.typeRef(prefix, sym, Nil)
          case _                                     => u.appliedType(sym, Nil)
        }
      }
      else { // is a term
        if (sym.hasAllFlags(Flags.PackageFlags)) {
          u.typeRef(u.NoPrefix, sym, Nil)
        } else {
          u.singleType(prefix, sym)
        }
      }
    }

    def TypeRef(prefix: Type, name: TastyName.TypeName)(implicit ctx: Context): Type =
      TypeRefIn(prefix, prefix, name)

    def TypeRefIn(prefix: Type, space: Type, name: TastyName.TypeName)(implicit ctx: Context): Type = {
      import scala.tools.tasty.TastyName._

      def doLookup = lookupTypeFrom(space)(prefix, name)

      val preSym = prefix.typeSymbol

      // we escape some types in the scala package especially
      if (preSym === u.definitions.ScalaPackage) {
        name match {
          case TypeName(SimpleName(raw @ SyntheticScala3Type())) => raw match {
            case tpnme.And                                                    => AndTpe
            case tpnme.Or                                                     => unionIsUnsupported
            case tpnme.ContextFunctionN(n)                                    => ContextFunctionType(n.toInt)
            case tpnme.FunctionN(n)              if (n.toInt > 22)            => FunctionXXLType(n.toInt)
            case tpnme.TupleCons                                              => genTupleIsUnsupported("scala.*:")
            case tpnme.Tuple                     if !ctx.mode.is(ReadParents) => genTupleIsUnsupported("scala.Tuple")
            case tpnme.AnyKind                                                => u.definitions.AnyTpe
            case tpnme.Matchable                                              => u.definitions.AnyTpe
            case tpnme.ErasedContextFunctionN(n) if n.toInt > 0               => ErasedContextFunctionType(n.toInt)
            case tpnme.ErasedFunctionN(n)                                     => ErasedFunctionType(n.toInt)
            case _                                                            => doLookup
          }

          case _ => doLookup
        }
      }
      else {
        if (ctx.isJava && preSym === u.definitions.JavaLangPackage) {
          name match {
            case TypeName(SimpleName(tpnme.Object)) => ObjectTpeJava // Object =:= scala.Any in Java.
            case _                                  => doLookup
          }
        } else {
          doLookup
        }
      }
    }

    def TermRef(prefix: Type, name: TastyName)(implicit ctx: Context): Type =
      TermRefIn(prefix, prefix, name)

    def TermRefIn(prefix: Type, space: Type, name: TastyName)(implicit ctx: Context): Type =
      lookupTypeFrom(space)(prefix, name.toTermName)

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
  private object NameErasure {

    def isRepeatedParam(self: Type): Boolean =
      self.typeSymbol eq u.definitions.RepeatedParamClass

    /** Translate a type of the form From[T] to either To[T] or To[? <: T] (if `wildcardArg` is set). Keep other types as they are.
     *  `from` and `to` must be static classes, both with one type parameter, and the same variance.
     *  Do the same for by name types => From[T] and => To[T]
     */
    def translateParameterized(self: Type)(from: u.ClassSymbol, to: u.ClassSymbol, wildcardArg: Boolean): Type = self match {
      case u.NullaryMethodType(tp) =>
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
          u.appliedType(to, arg1 :: Nil)
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
   * @see https://github.com/scala/scala3/issues/7688
   */
  case object AndTpe extends Type

  case class ErasedFunctionType(arity: Int) extends Type {
    assert(arity > 0)
  }

  case class ErasedContextFunctionType(arity: Int) extends Type {
    assert(arity > 0)
  }

  case class ContextFunctionType(arity: Int) extends Type {
    assert(arity > 0)
  }

  case class FunctionXXLType(arity: Int) extends Type {
    assert(arity > 22)
  }

  private val SyntheticScala3Type =
    raw"^(?:&|\||AnyKind|(?:Erased)?(?:Context)?Function\d+|\*:|Tuple|Matchable)$$".r

  sealed abstract trait TastyRepr extends u.Type {
    def tflags: TastyFlagSet
    final def unsupportedFlags: TastyFlagSet = tflags & FlagSets.TastyOnlyFlags
  }

  abstract class TastyCompleter(
      isClass: Boolean,
      tflags: TastyFlagSet
  )(implicit capturedCtx: Context)
      extends BaseTastyCompleter(tflags) {
    override final val decls: u.Scope = if (isClass) u.newScope else u.EmptyScope
  }

  private[TypeOps] class CopyCompleter(
      underlying: u.TermSymbol,
      tflags: TastyFlagSet
  )(implicit ctx: Context)
      extends BaseTastyCompleter(tflags) {
    def computeInfo(sym: Symbol)(implicit ctx: Context): Unit = {
      underlying.ensureCompleted(CopySym)
      sym.info = underlying.tpe
      underlying.attachments.all.foreach(sym.updateAttachment(_))
    }
  }

  /** This completer ensures that if the "fake" singleton enum module class
   *  is completed first, that it completes the module symbol which
   *  then completes the module class.
   */
  private[TypeOps] class SingletonEnumModuleClassCompleter(
      enumValue: u.TermSymbol,
      tflags: TastyFlagSet
  )(implicit ctx: Context)
      extends BaseTastyCompleter(tflags) {
    def computeInfo(sym: Symbol)(implicit ctx: Context): Unit = {
      enumValue.ensureCompleted(EnumProxy)
    }
  }

  private[TypeOps] class LocalSealedChildProxyCompleter(
      parent: Symbol,
      tflags: TastyFlagSet
  )(implicit ctx: Context)
      extends BaseTastyCompleter(tflags) {
    def computeInfo(sym: Symbol)(implicit ctx: Context): Unit = {
      sym.info = defn.ClassInfoType(parent.tpe_* :: Nil, sym) // TODO [tasty]: check if tpe_* forces
    }
  }

  private[TypeOps] final class LambdaParamCompleter(
      flags: TastyFlagSet,
      idx: Int,
      infoDb: Int => Type,
  )(implicit ctx: Context)
      extends BaseTastyCompleter(flags) {
    override def computeInfo(denot: Symbol)(implicit ctx: Context): Unit =
      denot.info = infoDb(idx)
  }

  abstract class BaseTastyCompleter(
      final val tflags: TastyFlagSet
  )(implicit capturedCtx: Context)
      extends u.LazyType
      with TastyRepr
      with u.FlagAgnosticCompleter {

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

  private[bridge] def lookupTypeFrom(owner: Type)(pre: Type, tname: TastyName)(implicit ctx: Context): Type =
    defn.NamedType(pre, lookupSymbol(owner, tname))

  private def lambdaResultType(resType: Type): Type = resType match {
    case res: LambdaPolyType => res.toNested
    case res                 => res
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

  /** The given type, unless `sym` is a constructor, in which case the
   *  type of the constructed instance is returned
   */
  def effectiveResultType(sym: Symbol, givenTp: Type): Type =
    if (sym.name == u.nme.CONSTRUCTOR) sym.owner.tpe
    else givenTp

  /** Lazy thread unsafe non-nullable value that can not be re-entered */
  private[bridge] final class SyncRef[A](private var compute: () => A) {
    private var out: A = _
    private var entered: Boolean = false

    def apply(): A = {
      if (entered) {
        assert(out != null, "cyclic completion of SyncRef")
      }
      else {
        entered = true
        val result = compute()
        compute = null
        assert(result != null, "SyncRef is non-nullable")
        out = result
      }
      out
    }
  }

  object MethodTermLambda extends TermLambdaFactory {

    type ThisLambda = MethodTermLambda

    protected def apply(
        params: ArraySeq[TastyName],
        flags: TastyFlagSet,
        paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
        resultTypeOp: () => Type,
        registerCallback: Type => Unit,
    )(implicit ctx: Context): ThisLambda = {
      new MethodTermLambda(params, paramInfosOp, resultTypeOp, flags, registerCallback)
    }

  }

  private[TypeOps] final class MethodTermLambda(
      paramTNames: ArraySeq[TastyName],
      paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
      resultTypeOp: () => Type,
      flags: TastyFlagSet,
      registerCallback: Type => Unit,
  )(implicit ctx: Context)
      extends TermLambda("MethodTermLambda")(paramTNames, paramInfosOp, resultTypeOp, flags)(registerCallback) {

    protected def canonical(ps: List[Symbol], res: Type): Type = u.MethodType(ps, res)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[MethodTermLambda]
  }

  object HKTypeLambda extends TypeLambdaFactory {

    type ThisLambda = HKTypeLambda

    protected def apply(
        params: ArraySeq[TastyName.TypeName],
        flags: TastyFlagSet,
        paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
        resultTypeOp: () => Type,
        registerCallback: Type => Unit,
    )(implicit ctx: Context): ThisLambda = {
      new HKTypeLambda(params, flags, paramInfosOp, resultTypeOp, registerCallback)
    }
  }

  private[TypeOps] final class HKTypeLambda(
      paramTNames: ArraySeq[TastyName.TypeName],
      flags: TastyFlagSet,
      paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
      resultTypeOp: () => Type,
      registerCallback: Type => Unit
  )(implicit ctx: Context)
      extends TypeLambda("HKTypeLambda")(paramTNames, flags, paramInfosOp, resultTypeOp)(registerCallback) {

    final override protected def normaliseResult(resType: Type): Type = lambdaResultType(resType)

    protected def canonical(ps: List[Symbol], res: Type): Type = new LambdaPolyType(ps, res)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[HKTypeLambda]
  }

  object PolyTypeLambda extends TypeLambdaFactory {

    type ThisLambda = PolyTypeLambda

    protected def apply(
        params: ArraySeq[TastyName.TypeName],
        flags: TastyFlagSet,
        paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
        resultTypeOp: () => Type,
        registerCallback: Type => Unit,
    )(implicit ctx: Context): ThisLambda = {
      new PolyTypeLambda(params, flags, paramInfosOp, resultTypeOp, registerCallback)
    }
  }

  private[TypeOps] final class PolyTypeLambda(
      paramTNames: ArraySeq[TastyName.TypeName],
      flags: TastyFlagSet,
      paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
      resultTypeOp: () => Type,
      registerCallback: Type => Unit
  )(implicit ctx: Context)
      extends TypeLambda("PolyTypeLambda")(paramTNames, flags, paramInfosOp, resultTypeOp)(registerCallback) {

    protected def canonical(ps: List[Symbol], res: Type): Type = u.PolyType(ps, res)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[PolyTypeLambda]
  }

  private[TypeOps] abstract class TypeLambda(
      kind: String)(
      paramTNames: ArraySeq[TastyName.TypeName],
      flags: TastyFlagSet,
      paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
      resultTypeOp: () => Type)(
      registerCallback: Type => Unit
  )(implicit ctx: Context)
      extends LambdaType(kind)(paramTNames, paramInfosOp, resultTypeOp, flags)(registerCallback) {
    final override def typeParams: List[Symbol] = lambdaParams.toList
    final protected def normaliseParam(info: Type): Type = normaliseIfBounds(info)
  }

  private[TypeOps] abstract class TermLambda(
      kind: String)(
      paramTNames: ArraySeq[TastyName],
      paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
      resultTypeOp: () => Type,
      flags: TastyFlagSet)(
      registerCallback: Type => Unit
  )(implicit ctx: Context)
      extends LambdaType(kind)(paramTNames, paramInfosOp, resultTypeOp, flags)(registerCallback) {
    final override def params: List[Symbol] = lambdaParams.toList
    final protected def normaliseParam(info: Type): Type = info
  }

  private[TypeOps] abstract class LambdaType(
      kind: String)(
      paramTNames: ArraySeq[TastyName],
      paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
      resultTypeOp: () => Type,
      flags: TastyFlagSet)(
      registerCallback: Type => Unit
  )(implicit ctx: Context) extends AbstractLambdaType(kind) {

    protected def normaliseParam(info: Type): Type
    protected def normaliseResult(resType: Type): Type = resType

    final val lambdaParams: ArraySeq[Symbol] = {
      val paramInfoDb = new SyncRef(() => paramInfosOp(this.lambdaParams))
      def infoAt(idx: Int) = normaliseParam(paramInfoDb()(idx))

      paramTNames.zipWithIndex.map { case (tname, idx) =>
        ctx.newLambdaParameter(tname, flags, idx, infoAt)
      }
    }

    registerCallback(this)

    final val resType: Type = normaliseResult(resultTypeOp())

  }

  private[TypeOps] abstract class AbstractLambdaType(override val productPrefix: String)
      extends Type
         with Product
         with Serializable {

    def lambdaParams: ArraySeq[Symbol]
    def resType: Type

    final override def etaExpand: Type = {
      lambdaParams.foreach(_.info) // force locally
      canonical(lambdaParams.toList, resType)
    }

    protected def canonical(ps: List[Symbol], res: Type): Type

    override final def productArity: Int = 2

    override final def productElement(n: Int): Any = n match {
      case 0 => lambdaParams
      case 1 => resType
      case _ => throw new IndexOutOfBoundsException(n.toString)
    }

    override final def equals(that: Any): Boolean = that match {
      case that: AbstractLambdaType =>
        (that.canEqual(self)
          && that.lambdaParams == lambdaParams
          && that.resType == resType)
      case _ => false
    }

  }

  abstract class LambdaFactory[N <: TastyName] {

    type ThisLambda <: LambdaType

    protected def apply(
        params: ArraySeq[N],
        flags: TastyFlagSet,
        paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
        resultTypeOp: () => Type,
        registerCallback: Type => Unit,
    )(implicit ctx: Context): ThisLambda

  }

  object LambdaFactory {
    final def parse[N <: TastyName](
        factory: LambdaFactory[N],
        params: ArraySeq[N],
        flags: TastyFlagSet)(
        paramInfosOp: ArraySeq[Symbol] => ArraySeq[Type],
        resultTypeOp: () => Type,
        registerCallback: Type => Unit,
    )(implicit ctx: Context): Type =
      factory(params, flags, paramInfosOp, resultTypeOp, registerCallback)
        .etaExpand // turn the LambdaType into something the compiler understands
        .tap(registerCallback) // we should replace the type at start as it has been expanded
  }

  abstract class TermLambdaFactory extends LambdaFactory[TastyName]
  abstract class TypeLambdaFactory extends LambdaFactory[TastyName.TypeName]

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

}
