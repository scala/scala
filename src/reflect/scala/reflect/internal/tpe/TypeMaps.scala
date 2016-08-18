package scala
package reflect
package internal
package tpe

import scala.collection.{ mutable, immutable }
import Flags._
import scala.annotation.tailrec
import Variance._

private[internal] trait TypeMaps {
  self: SymbolTable =>
  import definitions._

  /** Normalize any type aliases within this type (@see Type#normalize).
    *  Note that this depends very much on the call to "normalize", not "dealias",
    *  so it is no longer carries the too-stealthy name "deAlias".
    */
  object normalizeAliases extends TypeMap {
    def apply(tp: Type): Type = mapOver(tp match {
      case TypeRef(_, sym, _) if sym.isAliasType && tp.isHigherKinded => logResult(s"Normalized type alias function $tp")(tp.normalize)
      case TypeRef(_, sym, _) if sym.isAliasType                      => tp.normalize
      case tp                                                         => tp
    })
  }

  /** Remove any occurrence of type <singleton> from this type and its parents */
  object dropSingletonType extends TypeMap {
    def apply(tp: Type): Type = {
      tp match {
        case TypeRef(_, SingletonClass, _) =>
          AnyTpe
        case tp1 @ RefinedType(parents, decls) =>
          parents filter (_.typeSymbol != SingletonClass) match {
            case Nil                       => AnyTpe
            case p :: Nil if decls.isEmpty => mapOver(p)
            case ps                        => mapOver(copyRefinedType(tp1, ps, decls))
          }
        case tp1 =>
          mapOver(tp1)
      }
    }
  }

  /** Type with all top-level occurrences of abstract types replaced by their bounds */
  object abstractTypesToBounds extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(_, sym, _) if sym.isAliasType    => apply(tp.dealias)
      case TypeRef(_, sym, _) if sym.isAbstractType => apply(tp.bounds.hi)
      case rtp @ RefinedType(parents, decls)        => copyRefinedType(rtp, parents mapConserve this, decls)
      case AnnotatedType(_, _)                      => mapOver(tp)
      case _                                        => tp             // no recursion - top level only
    }
  }

  // Set to true for A* => Seq[A]
  //   (And it will only rewrite A* in method result types.)
  //   This is the pre-existing behavior.
  // Or false for Seq[A] => Seq[A]
  //   (It will rewrite A* everywhere but method parameters.)
  //   This is the specified behavior.
  protected def etaExpandKeepsStar = false

  /** Turn any T* types into Seq[T] except when
    *  in method parameter position.
    */
  object dropIllegalStarTypes extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case MethodType(params, restpe) =>
        // Not mapping over params
        val restpe1 = apply(restpe)
        if (restpe eq restpe1) tp
        else MethodType(params, restpe1)
      case TypeRef(_, RepeatedParamClass, arg :: Nil) =>
        seqType(arg)
      case _ =>
        if (etaExpandKeepsStar) tp else mapOver(tp)
    }
  }

  trait AnnotationFilter extends TypeMap {
    def keepAnnotation(annot: AnnotationInfo): Boolean

    override def mapOver(annot: AnnotationInfo) =
      if (keepAnnotation(annot)) super.mapOver(annot)
      else UnmappableAnnotation
  }

  trait KeepOnlyTypeConstraints extends AnnotationFilter {
    // filter keeps only type constraint annotations
    def keepAnnotation(annot: AnnotationInfo) = annot matches TypeConstraintClass
  }

  // todo. move these into scala.reflect.api

  /** A prototype for mapping a function over all possible types
    */
  abstract class TypeMap(trackVariance: Boolean) extends (Type => Type) {
    def this() = this(trackVariance = false)
    def apply(tp: Type): Type

    private[this] var _variance: Variance = if (trackVariance) Covariant else Invariant

    def variance_=(x: Variance) = { assert(trackVariance, this) ; _variance = x }
    def variance = _variance

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case tr @ TypeRef(pre, sym, args) =>
        val pre1 = this(pre)
        val args1 = (
          if (trackVariance && args.nonEmpty && !variance.isInvariant && sym.typeParams.nonEmpty)
            mapOverArgs(args, sym.typeParams)
          else
            args mapConserve this
          )
        if ((pre1 eq pre) && (args1 eq args)) tp
        else copyTypeRef(tp, pre1, tr.coevolveSym(pre1), args1)
      case ThisType(_) => tp
      case SingleType(pre, sym) =>
        if (sym.isPackageClass) tp // short path
        else {
          val pre1 = this(pre)
          if (pre1 eq pre) tp
          else singleType(pre1, sym)
        }
      case MethodType(params, result) =>
        val params1 = flipped(mapOver(params))
        val result1 = this(result)
        if ((params1 eq params) && (result1 eq result)) tp
        else copyMethodType(tp, params1, result1.substSym(params, params1))
      case PolyType(tparams, result) =>
        val tparams1 = flipped(mapOver(tparams))
        val result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else PolyType(tparams1, result1.substSym(tparams, tparams1))
      case NullaryMethodType(result) =>
        val result1 = this(result)
        if (result1 eq result) tp
        else NullaryMethodType(result1)
      case ConstantType(_) => tp
      case SuperType(thistp, supertp) =>
        val thistp1 = this(thistp)
        val supertp1 = this(supertp)
        if ((thistp1 eq thistp) && (supertp1 eq supertp)) tp
        else SuperType(thistp1, supertp1)
      case TypeBounds(lo, hi) =>
        val lo1 = flipped(this(lo))
        val hi1 = this(hi)
        if ((lo1 eq lo) && (hi1 eq hi)) tp
        else TypeBounds(lo1, hi1)
      case BoundedWildcardType(bounds) =>
        val bounds1 = this(bounds)
        if (bounds1 eq bounds) tp
        else BoundedWildcardType(bounds1.asInstanceOf[TypeBounds])
      case rtp @ RefinedType(parents, decls) =>
        val parents1 = parents mapConserve this
        val decls1 = mapOver(decls)
        copyRefinedType(rtp, parents1, decls1)
      case ExistentialType(tparams, result) =>
        val tparams1 = mapOver(tparams)
        val result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else newExistentialType(tparams1, result1.substSym(tparams, tparams1))
      case OverloadedType(pre, alts) =>
        val pre1 = if (pre.isInstanceOf[ClassInfoType]) pre else this(pre)
        if (pre1 eq pre) tp
        else OverloadedType(pre1, alts)
      case AntiPolyType(pre, args) =>
        val pre1 = this(pre)
        val args1 = args mapConserve this
        if ((pre1 eq pre) && (args1 eq args)) tp
        else AntiPolyType(pre1, args1)
      case tv@TypeVar(_, constr) =>
        if (constr.instValid) this(constr.inst)
        else tv.applyArgs(mapOverArgs(tv.typeArgs, tv.params))  //@M !args.isEmpty implies !typeParams.isEmpty
      case AnnotatedType(annots, atp) =>
        val annots1 = mapOverAnnotations(annots)
        val atp1 = this(atp)
        if ((annots1 eq annots) && (atp1 eq atp)) tp
        else if (annots1.isEmpty) atp1
        else AnnotatedType(annots1, atp1)
      /*
            case ErrorType => tp
            case WildcardType => tp
            case NoType => tp
            case NoPrefix => tp
            case ErasedSingleType(sym) => tp
      */
      case _ =>
        tp
      // throw new Error("mapOver inapplicable for " + tp);
    }

    def withVariance[T](v: Variance)(body: => T): T = {
      val saved = variance
      variance = v
      try body finally variance = saved
    }
    @inline final def flipped[T](body: => T): T = {
      if (trackVariance) variance = variance.flip
      try body
      finally if (trackVariance) variance = variance.flip
    }
    protected def mapOverArgs(args: List[Type], tparams: List[Symbol]): List[Type] = (
      if (trackVariance)
        map2Conserve(args, tparams)((arg, tparam) => withVariance(variance * tparam.variance)(this(arg)))
      else
        args mapConserve this
      )
    /** Applies this map to the symbol's info, setting variance = Invariant
      *  if necessary when the symbol is an alias.
      */
    private def applyToSymbolInfo(sym: Symbol): Type = {
      if (trackVariance && !variance.isInvariant && sym.isAliasType)
        withVariance(Invariant)(this(sym.info))
      else
        this(sym.info)
    }

    /** Called by mapOver to determine whether the original symbols can
      *  be returned, or whether they must be cloned.
      */
    protected def noChangeToSymbols(origSyms: List[Symbol]): Boolean = {
      @tailrec def loop(syms: List[Symbol]): Boolean = syms match {
        case Nil     => true
        case x :: xs => (x.info eq applyToSymbolInfo(x)) && loop(xs)
      }
      loop(origSyms)
    }

    /** Map this function over given scope */
    def mapOver(scope: Scope): Scope = {
      val elems = scope.toList
      val elems1 = mapOver(elems)
      if (elems1 eq elems) scope
      else newScopeWith(elems1: _*)
    }

    /** Map this function over given list of symbols */
    def mapOver(origSyms: List[Symbol]): List[Symbol] = {
      // fast path in case nothing changes due to map
      if (noChangeToSymbols(origSyms)) origSyms
      // map is not the identity --> do cloning properly
      else cloneSymbolsAndModify(origSyms, TypeMap.this)
    }

    def mapOver(annot: AnnotationInfo): AnnotationInfo = {
      val AnnotationInfo(atp, args, assocs) = annot
      val atp1  = mapOver(atp)
      val args1 = mapOverAnnotArgs(args)
      // there is no need to rewrite assocs, as they are constants

      if ((args eq args1) && (atp eq atp1)) annot
      else if (args1.isEmpty && args.nonEmpty) UnmappableAnnotation  // some annotation arg was unmappable
      else AnnotationInfo(atp1, args1, assocs) setPos annot.pos
    }

    def mapOverAnnotations(annots: List[AnnotationInfo]): List[AnnotationInfo] = {
      val annots1 = annots mapConserve mapOver
      if (annots1 eq annots) annots
      else annots1 filterNot (_ eq UnmappableAnnotation)
    }

    /** Map over a set of annotation arguments.  If any
      *  of the arguments cannot be mapped, then return Nil.  */
    def mapOverAnnotArgs(args: List[Tree]): List[Tree] = {
      val args1 = args mapConserve mapOver
      if (args1 contains UnmappableTree) Nil
      else args1
    }

    def mapOver(tree: Tree): Tree =
      mapOver(tree, () => return UnmappableTree)

    /** Map a tree that is part of an annotation argument.
      *  If the tree cannot be mapped, then invoke giveup().
      *  The default is to transform the tree with
      *  TypeMapTransformer.
      */
    def mapOver(tree: Tree, giveup: ()=>Nothing): Tree =
      (new TypeMapTransformer).transform(tree)

    /** This transformer leaves the tree alone except to remap
      *  its types. */
    class TypeMapTransformer extends Transformer {
      override def transform(tree: Tree) = {
        val tree1 = super.transform(tree)
        val tpe1 = TypeMap.this(tree1.tpe)
        if ((tree eq tree1) && (tree.tpe eq tpe1))
          tree
        else
          tree1.shallowDuplicate.setType(tpe1)
      }
    }
  }

  abstract class TypeTraverser extends TypeMap {
    def traverse(tp: Type): Unit
    def apply(tp: Type): Type = { traverse(tp); tp }
  }

  abstract class TypeTraverserWithResult[T] extends TypeTraverser {
    def result: T
    def clear(): Unit
  }

  abstract class TypeCollector[T](initial: T) extends TypeTraverser {
    var result: T = _
    def collect(tp: Type) = {
      result = initial
      traverse(tp)
      result
    }
  }

  /** The raw to existential map converts a ''raw type'' to an existential type.
    *  It is necessary because we might have read a raw type of a
    *  parameterized Java class from a class file. At the time we read the type
    *  the corresponding class file might still not be read, so we do not
    *  know what the type parameters of the type are. Therefore
    *  the conversion of raw types to existential types might not have taken place
    *  in ClassFileparser.sigToType (where it is usually done).
    */
  def rawToExistential = new TypeMap {
    private var expanded = immutable.Set[Symbol]()
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, List()) if isRawIfWithoutArgs(sym) =>
        if (expanded contains sym) AnyRefTpe
        else try {
          expanded += sym
          val eparams = mapOver(typeParamsToExistentials(sym))
          existentialAbstraction(eparams, typeRef(apply(pre), sym, eparams map (_.tpe)))
        } finally {
          expanded -= sym
        }
      case _ =>
        mapOver(tp)
    }
  }
  /***
    *@M: I think this is more desirable, but Martin prefers to leave raw-types as-is as much as possible
    object rawToExistentialInJava extends TypeMap {
      def apply(tp: Type): Type = tp match {
        // any symbol that occurs in a java sig, not just java symbols
        // see https://issues.scala-lang.org/browse/SI-2454?focusedCommentId=46618
        case TypeRef(pre, sym, List()) if !sym.typeParams.isEmpty =>
          val eparams = typeParamsToExistentials(sym, sym.typeParams)
          existentialAbstraction(eparams, TypeRef(pre, sym, eparams map (_.tpe)))
        case _ =>
          mapOver(tp)
      }
    }
    */

  /** Used by existentialAbstraction.
    */
  class ExistentialExtrapolation(tparams: List[Symbol]) extends TypeMap(trackVariance = true) {
    private val occurCount = mutable.HashMap[Symbol, Int]()
    private def countOccs(tp: Type) = {
      tp foreach {
        case TypeRef(_, sym, _) =>
          if (tparams contains sym)
            occurCount(sym) += 1
        case _ => ()
      }
    }
    def extrapolate(tpe: Type): Type = {
      tparams foreach (t => occurCount(t) = 0)
      countOccs(tpe)
      for (tparam <- tparams)
        countOccs(tparam.info)

      apply(tpe)
    }

    /** If these conditions all hold:
      *   1) we are in covariant (or contravariant) position
      *   2) this type occurs exactly once in the existential scope
      *   3) the widened upper (or lower) bound of this type contains no references to tparams
      *  Then we replace this lone occurrence of the type with the widened upper (or lower) bound.
      *  All other types pass through unchanged.
      */
    def apply(tp: Type): Type = {
      val tp1 = mapOver(tp)
      if (variance.isInvariant) tp1
      else tp1 match {
        case TypeRef(pre, sym, args) if tparams contains sym =>
          val repl = if (variance.isPositive) dropSingletonType(tp1.bounds.hi) else tp1.bounds.lo
          val count = occurCount(sym)
          val containsTypeParam = tparams exists (repl contains _)
          def msg = {
            val word = if (variance.isPositive) "upper" else "lower"
            s"Widened lone occurrence of $tp1 inside existential to $word bound"
          }
          if (!repl.typeSymbol.isBottomClass && count == 1 && !containsTypeParam)
            debuglogResult(msg)(repl)
          else
            tp1
        case _ =>
          tp1
      }
    }
    override def mapOver(tp: Type): Type = tp match {
      case SingleType(pre, sym) =>
        if (sym.isPackageClass) tp // short path
        else {
          val pre1 = this(pre)
          if ((pre1 eq pre) || !pre1.isStable) tp
          else singleType(pre1, sym)
        }
      case _ => super.mapOver(tp)
    }

    // Do not discard the types of existential ident's. The
    // symbol of the Ident itself cannot be listed in the
    // existential's parameters, so the resulting existential
    // type would be ill-formed.
    override def mapOver(tree: Tree) = tree match {
      case Ident(_) if tree.tpe.isStable => tree
      case _                             => super.mapOver(tree)
    }
  }

  /**
   * Get rid of BoundedWildcardType where variance allows us to do so.
   * Invariant: `wildcardExtrapolation(tp) =:= tp`
   *
   * For example, the MethodType given by `def bla(x: (_ >: String)): (_ <: Int)`
   * is both a subtype and a supertype of `def bla(x: String): Int`.
   */
  object wildcardExtrapolation extends TypeMap(trackVariance = true) {
    def apply(tp: Type): Type =
      tp match {
        case BoundedWildcardType(TypeBounds(lo, AnyTpe)) if variance.isContravariant => lo
        case BoundedWildcardType(TypeBounds(NothingTpe, hi)) if variance.isCovariant => hi
        case tp => mapOver(tp)
      }
  }

  /** Might the given symbol be important when calculating the prefix
    *  of a type? When tp.asSeenFrom(pre, clazz) is called on `tp`,
    *  the result will be `tp` unchanged if `pre` is trivial and `clazz`
    *  is a symbol such that isPossiblePrefix(clazz) == false.
    */
  def isPossiblePrefix(clazz: Symbol) = clazz.isClass && !clazz.isPackageClass

  protected[internal] def skipPrefixOf(pre: Type, clazz: Symbol) = (
    (pre eq NoType) || (pre eq NoPrefix) || !isPossiblePrefix(clazz)
    )

  @deprecated("use new AsSeenFromMap instead", "2.12.0")
  final def newAsSeenFromMap(pre: Type, clazz: Symbol): AsSeenFromMap = new AsSeenFromMap(pre, clazz)

  /** A map to compute the asSeenFrom method.
    */
  class AsSeenFromMap(seenFromPrefix0: Type, seenFromClass: Symbol) extends TypeMap with KeepOnlyTypeConstraints {
    private val seenFromPrefix: Type = if (seenFromPrefix0.typeSymbolDirect.hasPackageFlag && !seenFromClass.hasPackageFlag)
      seenFromPrefix0.packageObject.typeOfThis
    else seenFromPrefix0
    // Some example source constructs relevant in asSeenFrom:
    //
    // object CaptureThis {
    //   trait X[A] { def f: this.type = this }
    //   class Y[A] { def f: this.type = this }
    //   // Created new existential to represent This(CaptureThis.X) seen from CaptureThis.X[B]: type _1.type <: CaptureThis.X[B] with Singleton
    //   def f1[B] = new X[B] { }
    //   // TODO - why is the behavior different when it's a class?
    //   def f2[B] = new Y[B] { }
    // }
    // class CaptureVal[T] {
    //   val f: java.util.List[_ <: T] = null
    //   // Captured existential skolem for type _$1 seen from CaptureVal.this.f.type: type _$1
    //   def g = f get 0
    // }
    // class ClassParam[T] {
    //   // AsSeenFromMap(Inner.this.type, class Inner)/classParameterAsSeen(T)#loop(ClassParam.this.type, class ClassParam)
    //   class Inner(lhs: T) { def f = lhs }
    // }
    def capturedParams: List[Symbol]  = _capturedParams
    def capturedSkolems: List[Symbol] = _capturedSkolems

    def apply(tp: Type): Type = tp match {
      case tp @ ThisType(_)                                            => thisTypeAsSeen(tp)
      case tp @ SingleType(_, sym)                                     => if (sym.isPackageClass) tp else singleTypeAsSeen(tp)
      case tp @ TypeRef(_, sym, _) if isTypeParamOfEnclosingClass(sym) => classParameterAsSeen(tp)
      case _                                                           => mapOver(tp)
    }

    private var _capturedSkolems: List[Symbol] = Nil
    private var _capturedParams: List[Symbol]  = Nil
    private val isStablePrefix = seenFromPrefix.isStable

    // isBaseClassOfEnclosingClassOrInfoIsNotYetComplete would be a more accurate
    // but less succinct name.
    private def isBaseClassOfEnclosingClass(base: Symbol) = {
      def loop(encl: Symbol): Boolean = (
        isPossiblePrefix(encl)
          && ((encl isSubClass base) || loop(encl.owner.enclClass))
        )
      // The hasCompleteInfo guard is necessary to avoid cycles during the typing
      // of certain classes, notably ones defined inside package objects.
      !base.hasCompleteInfo || loop(seenFromClass)
    }

    /** Is the symbol a class type parameter from one of the enclosing
      *  classes, or a base class of one of them?
      */
    private def isTypeParamOfEnclosingClass(sym: Symbol): Boolean = (
      sym.isTypeParameter
        && sym.owner.isClass
        && isBaseClassOfEnclosingClass(sym.owner)
      )

    /** Creates an existential representing a type parameter which appears
      *  in the prefix of a ThisType.
      */
    protected def captureThis(pre: Type, clazz: Symbol): Type = {
      capturedParams find (_.owner == clazz) match {
        case Some(p) => p.tpe
        case _       =>
          val qvar = clazz freshExistential nme.SINGLETON_SUFFIX setInfo singletonBounds(pre)
          _capturedParams ::= qvar
          debuglog(s"Captured This(${clazz.fullNameString}) seen from $seenFromPrefix: ${qvar.defString}")
          qvar.tpe
      }
    }
    protected def captureSkolems(skolems: List[Symbol]) {
      for (p <- skolems; if !(capturedSkolems contains p)) {
        debuglog(s"Captured $p seen from $seenFromPrefix")
        _capturedSkolems ::= p
      }
    }

    /** Find the type argument in an applied type which corresponds to a type parameter.
      *  The arguments are required to be related as follows, through intermediary `clazz`.
      *  An exception will be thrown if this is violated.
      *
      *  @param   lhs    its symbol is a type parameter of `clazz`
      *  @param   rhs    a type application constructed from `clazz`
      */
    private def correspondingTypeArgument(lhs: Type, rhs: Type): Type = {
      val TypeRef(_, lhsSym, lhsArgs) = lhs
      val TypeRef(_, rhsSym, rhsArgs) = rhs
      require(lhsSym.owner == rhsSym, s"$lhsSym is not a type parameter of $rhsSym")

      // Find the type parameter position; we'll use the corresponding argument.
      // Why are we checking by name rather than by equality? Because for
      // reasons which aren't yet fully clear, we can arrive here holding a type
      // parameter whose owner is rhsSym, and which shares the name of an actual
      // type parameter of rhsSym, but which is not among the type parameters of
      // rhsSym. One can see examples of it at SI-4365.
      val argIndex = rhsSym.typeParams indexWhere (lhsSym.name == _.name)
      // don't be too zealous with the exceptions, see #2641
      if (argIndex < 0 && rhs.parents.exists(typeIsErroneous))
        ErrorType
      else {
        // It's easy to get here when working on hardcore type machinery (not to
        // mention when not doing so, see above) so let's provide a standout error.
        def own_s(s: Symbol) = s.nameString + " in " + s.owner.nameString
        def explain =
          sm"""|   sought  ${own_s(lhsSym)}
               | classSym  ${own_s(rhsSym)}
               |  tparams  ${rhsSym.typeParams map own_s mkString ", "}
               |"""

        if (!rhsArgs.isDefinedAt(argIndex))
          abort(s"Something is wrong: cannot find $lhs in applied type $rhs\n" + explain)
        else {
          val targ   = rhsArgs(argIndex)
          // @M! don't just replace the whole thing, might be followed by type application
          val result = appliedType(targ, lhsArgs mapConserve this)
          def msg = s"Created $result, though could not find ${own_s(lhsSym)} among tparams of ${own_s(rhsSym)}"
          if (!rhsSym.typeParams.contains(lhsSym))
            devWarning(s"Inconsistent tparam/owner views: had to fall back on names\n$msg\n$explain")

          result
        }
      }
    }

    // 0) @pre: `classParam` is a class type parameter
    // 1) Walk the owner chain of `seenFromClass` until we find the class which owns `classParam`
    // 2) Take the base type of the prefix at that point with respect to the owning class
    // 3) Solve for the type parameters through correspondence with the type args of the base type
    //
    // Only class type parameters (and not skolems) are considered, because other type parameters
    // are not influenced by the prefix through which they are seen. Note that type params of
    // anonymous type functions, which currently can only arise from normalising type aliases, are
    // owned by the type alias of which they are the eta-expansion.
    private def classParameterAsSeen(classParam: Type): Type = {
      val TypeRef(_, tparam, _) = classParam

      def loop(pre: Type, clazz: Symbol): Type = {
        // have to deconst because it may be a Class[T]
        def nextBase = (pre baseType clazz).deconst
        //@M! see test pos/tcpoly_return_overriding.scala why mapOver is necessary
        if (skipPrefixOf(pre, clazz))
          mapOver(classParam)
        else if (!matchesPrefixAndClass(pre, clazz)(tparam.owner))
          loop(nextBase.prefix, clazz.owner)
        else nextBase match {
          case NoType                         => loop(NoType, clazz.owner) // backstop for SI-2797, must remove `SingletonType#isHigherKinded` and run pos/t2797.scala to get here.
          case applied @ TypeRef(_, _, _)     => correspondingTypeArgument(classParam, applied)
          case ExistentialType(eparams, qtpe) => captureSkolems(eparams) ; loop(qtpe, clazz)
          case t                              => abort(s"$tparam in ${tparam.owner} cannot be instantiated from ${seenFromPrefix.widen}")
        }
      }
      loop(seenFromPrefix, seenFromClass)
    }

    // Does the candidate symbol match the given prefix and class?
    private def matchesPrefixAndClass(pre: Type, clazz: Symbol)(candidate: Symbol) = (clazz == candidate) && {
      val pre1 = pre match {
        case tv: TypeVar =>
          // Needed with existentials in prefixes, e.g. test/files/pos/typevar-in-prefix.scala
          // Perhaps the base type sequence of a type var should include its bounds?
          tv.origin
        case _ => pre
      }
      // widen needed (at least) because of https://github.com/scala/scala-dev/issues/166
      (
        if (clazz.isRefinementClass)
          // base type seqs of aliases over refinement types have copied refinement types based on beta reduction
          // for reliable lookup we need to consult the base type of the type symbol. (example: pos/t8177b.scala)
          pre1.widen.typeSymbol isSubClass clazz
        else
          // In the general case, we look at the base type sequence of the prefix itself,
          // which can have more concrete base classes than `.typeSymbol.baseClasses` (example: t5294, t6161)
          pre1.widen.baseTypeIndex(clazz) != -1
      )
    }

    // Whether the annotation tree currently being mapped over has had a This(_) node rewritten.
    private[this] var wroteAnnotation = false
    private object annotationArgRewriter extends TypeMapTransformer {
      private def matchesThis(thiz: Symbol) = matchesPrefixAndClass(seenFromPrefix, seenFromClass)(thiz)

      // what symbol should really be used?
      private def newThis(): Tree = {
        wroteAnnotation = true
        val presym      = seenFromPrefix.widen.typeSymbol
        val thisSym     = presym.owner.newValue(presym.name.toTermName, presym.pos) setInfo seenFromPrefix
        gen.mkAttributedQualifier(seenFromPrefix, thisSym)
      }

      /** Rewrite `This` trees in annotation argument trees */
      override def transform(tree: Tree): Tree = super.transform(tree) match {
        case This(_) if matchesThis(tree.symbol) => newThis()
        case tree                                => tree
      }
    }

    // This becomes considerably cheaper if we optimize for the common cases:
    // where the prefix is stable and where no This nodes are rewritten. If
    // either is true, then we don't need to worry about calling giveup. So if
    // the prefix is unstable, use a stack variable to indicate whether the tree
    // was touched. This takes us to one allocation per AsSeenFromMap rather
    // than an allocation on every call to mapOver, and no extra work when the
    // tree only has its types remapped.
    override def mapOver(tree: Tree, giveup: ()=>Nothing): Tree = {
      if (isStablePrefix)
        annotationArgRewriter transform tree
      else {
        val saved = wroteAnnotation
        wroteAnnotation = false
        try annotationArgRewriter transform tree
        finally if (wroteAnnotation) giveup() else wroteAnnotation = saved
      }
    }

    private def thisTypeAsSeen(tp: ThisType): Type = {
      def loop(pre: Type, clazz: Symbol): Type = {
        val pre1 = pre match {
          case SuperType(thistpe, _) => thistpe
          case _                     => pre
        }
        if (skipPrefixOf(pre, clazz))
          mapOver(tp) // TODO - is mapOver necessary here?
        else if (!matchesPrefixAndClass(pre, clazz)(tp.sym))
          loop((pre baseType clazz).prefix, clazz.owner)
        else if (pre1.isStable)
          pre1
        else
          captureThis(pre1, clazz)
      }
      loop(seenFromPrefix, seenFromClass)
    }

    private def singleTypeAsSeen(tp: SingleType): Type = {
      val SingleType(pre, sym) = tp

      val pre1 = this(pre)
      if (pre1 eq pre) tp
      else if (pre1.isStable) singleType(pre1, sym)
      else pre1.memberType(sym).resultType //todo: this should be rolled into existential abstraction
    }

    override def toString = s"AsSeenFromMap($seenFromPrefix, $seenFromClass)"
  }

  /** A base class to compute all substitutions */
  abstract class SubstMap[T](from: List[Symbol], to: List[T]) extends TypeMap {
    // OPT this check was 2-3% of some profiles, demoted to -Xdev
    if (isDeveloper) assert(sameLength(from, to), "Unsound substitution from "+ from +" to "+ to)

    /** Are `sym` and `sym1` the same? Can be tuned by subclasses. */
    protected def matches(sym: Symbol, sym1: Symbol): Boolean = sym eq sym1

    /** Map target to type, can be tuned by subclasses */
    protected def toType(fromtp: Type, tp: T): Type

    protected def renameBoundSyms(tp: Type): Type = tp match {
      case MethodType(ps, restp) =>
        createFromClonedSymbols(ps, restp)((ps1, tp1) => copyMethodType(tp, ps1, renameBoundSyms(tp1)))
      case PolyType(bs, restp) =>
        createFromClonedSymbols(bs, restp)((ps1, tp1) => PolyType(ps1, renameBoundSyms(tp1)))
      case ExistentialType(bs, restp) =>
        createFromClonedSymbols(bs, restp)(newExistentialType)
      case _ =>
        tp
    }

    @tailrec private def subst(tp: Type, sym: Symbol, from: List[Symbol], to: List[T]): Type = (
      if (from.isEmpty) tp
      // else if (to.isEmpty) error("Unexpected substitution on '%s': from = %s but to == Nil".format(tp, from))
      else if (matches(from.head, sym)) toType(tp, to.head)
      else subst(tp, sym, from.tail, to.tail)
      )

    def apply(tp0: Type): Type = if (from.isEmpty) tp0 else {
      val boundSyms             = tp0.boundSyms
      val tp1                   = if (boundSyms.nonEmpty && (boundSyms exists from.contains)) renameBoundSyms(tp0) else tp0
      val tp                    = mapOver(tp1)
      def substFor(sym: Symbol) = subst(tp, sym, from, to)

      tp match {
        // @M
        // 1) arguments must also be substituted (even when the "head" of the
        // applied type has already been substituted)
        // example: (subst RBound[RT] from [type RT,type RBound] to
        // [type RT&,type RBound&]) = RBound&[RT&]
        // 2) avoid loops (which occur because alpha-conversion is
        // not performed properly imo)
        // e.g. if in class Iterable[a] there is a new Iterable[(a,b)],
        // we must replace the a in Iterable[a] by (a,b)
        // (must not recurse --> loops)
        // 3) replacing m by List in m[Int] should yield List[Int], not just List
        case TypeRef(NoPrefix, sym, args) =>
          val tcon = substFor(sym)
          if ((tp eq tcon) || args.isEmpty) tcon
          else appliedType(tcon.typeConstructor, args)
        case SingleType(NoPrefix, sym) =>
          substFor(sym)
        case ClassInfoType(parents, decls, sym) =>
          val parents1 = parents mapConserve this
          // We don't touch decls here; they will be touched when an enclosing TreeSubstituter
          // transforms the tree that defines them.
          if (parents1 eq parents) tp
          else ClassInfoType(parents1, decls, sym)
        case _ =>
          tp
      }
    }
  }

  /** A map to implement the `substSym` method. */
  class SubstSymMap(from: List[Symbol], to: List[Symbol]) extends SubstMap(from, to) {
    def this(pairs: (Symbol, Symbol)*) = this(pairs.toList.map(_._1), pairs.toList.map(_._2))

    protected def toType(fromtp: Type, sym: Symbol) = fromtp match {
      case TypeRef(pre, _, args) => copyTypeRef(fromtp, pre, sym, args)
      case SingleType(pre, _) => singleType(pre, sym)
    }
    @tailrec private def subst(sym: Symbol, from: List[Symbol], to: List[Symbol]): Symbol = (
      if (from.isEmpty) sym
      // else if (to.isEmpty) error("Unexpected substitution on '%s': from = %s but to == Nil".format(sym, from))
      else if (matches(from.head, sym)) to.head
      else subst(sym, from.tail, to.tail)
      )
    private def substFor(sym: Symbol) = subst(sym, from, to)

    override def apply(tp: Type): Type = (
      if (from.isEmpty) tp
      else tp match {
        case TypeRef(pre, sym, args) if pre ne NoPrefix =>
          val newSym = substFor(sym)
          // mapOver takes care of subst'ing in args
          mapOver ( if (sym eq newSym) tp else copyTypeRef(tp, pre, newSym, args) )
        // assert(newSym.typeParams.length == sym.typeParams.length, "typars mismatch in SubstSymMap: "+(sym, sym.typeParams, newSym, newSym.typeParams))
        case SingleType(pre, sym) if pre ne NoPrefix =>
          val newSym = substFor(sym)
          mapOver( if (sym eq newSym) tp else singleType(pre, newSym) )
        case _ =>
          super.apply(tp)
      }
      )

    object mapTreeSymbols extends TypeMapTransformer {
      val strictCopy = newStrictTreeCopier

      def termMapsTo(sym: Symbol) = from indexOf sym match {
        case -1   => None
        case idx  => Some(to(idx))
      }

      // if tree.symbol is mapped to another symbol, passes the new symbol into the
      // constructor `trans` and sets the symbol and the type on the resulting tree.
      def transformIfMapped(tree: Tree)(trans: Symbol => Tree) = termMapsTo(tree.symbol) match {
        case Some(toSym) => trans(toSym) setSymbol toSym setType tree.tpe
        case None => tree
      }

      // changes trees which refer to one of the mapped symbols. trees are copied before attributes are modified.
      override def transform(tree: Tree) = {
        // super.transform maps symbol references in the types of `tree`. it also copies trees where necessary.
        super.transform(tree) match {
          case id @ Ident(_) =>
            transformIfMapped(id)(toSym =>
              strictCopy.Ident(id, toSym.name))

          case sel @ Select(qual, name) =>
            transformIfMapped(sel)(toSym =>
              strictCopy.Select(sel, qual, toSym.name))

          case tree => tree
        }
      }
    }
    override def mapOver(tree: Tree, giveup: ()=>Nothing): Tree = {
      mapTreeSymbols.transform(tree)
    }
  }

  /** A map to implement the `subst` method. */
  class SubstTypeMap(val from: List[Symbol], val to: List[Type]) extends SubstMap(from, to) {
    protected def toType(fromtp: Type, tp: Type) = tp

    override def mapOver(tree: Tree, giveup: () => Nothing): Tree = {
      object trans extends TypeMapTransformer {
        override def transform(tree: Tree) = tree match {
          case Ident(name) =>
            from indexOf tree.symbol match {
              case -1   => super.transform(tree)
              case idx  =>
                val totpe = to(idx)
                if (totpe.isStable) tree.duplicate setType totpe
                else giveup()
            }
          case _ =>
            super.transform(tree)
        }
      }
      trans.transform(tree)
    }
  }

  /** A map to implement the `substThis` method. */
  class SubstThisMap(from: Symbol, to: Type) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case ThisType(sym) if (sym == from) => to
      case _ => mapOver(tp)
    }
  }

  class SubstWildcardMap(from: List[Symbol]) extends TypeMap {
    def apply(tp: Type): Type = try {
      tp match {
        case TypeRef(_, sym, _) if from contains sym =>
          BoundedWildcardType(sym.info.bounds)
        case _ =>
          mapOver(tp)
      }
    } catch {
      case ex: MalformedType =>
        WildcardType
    }
  }

  // dependent method types
  object IsDependentCollector extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (tp.isImmediatelyDependent) result = true
      else if (!result) mapOver(tp.dealias)
    }
  }

  object ApproximateDependentMap extends TypeMap {
    def apply(tp: Type): Type =
      if (tp.isImmediatelyDependent) WildcardType
      else mapOver(tp)
  }

  /** Note: This map is needed even for non-dependent method types, despite what the name might imply.
    */
  class InstantiateDependentMap(params: List[Symbol], actuals0: List[Type]) extends TypeMap with KeepOnlyTypeConstraints {
    private val actuals      = actuals0.toIndexedSeq
    private val existentials = new Array[Symbol](actuals.size)
    def existentialsNeeded: List[Symbol] = existentials.iterator.filter(_ ne null).toList

    private object StableArgTp {
      // type of actual arg corresponding to param -- if the type is stable
      def unapply(param: Symbol): Option[Type] = (params indexOf param) match {
        case -1  => None
        case pid =>
          val tp = actuals(pid)
          if (tp.isStable && (tp.typeSymbol != NothingClass)) Some(tp)
          else None
      }
    }

    /** Return the type symbol for referencing a parameter that's instantiated to an unstable actual argument.
     *
     * To soundly abstract over an unstable value (x: T) while retaining the most type information,
     * use `x.type forSome { type x.type <: T with Singleton}`
     * `typeOf[T].narrowExistentially(symbolOf[x])`.
     *
     * See also: captureThis in AsSeenFromMap.
     */
    private def existentialFor(pid: Int) = {
      if (existentials(pid) eq null) {
        val param = params(pid)
        existentials(pid) = (
          param.owner.newExistential(param.name.toTypeName append nme.SINGLETON_SUFFIX, param.pos, param.flags)
            setInfo singletonBounds(actuals(pid))
          )
      }
      existentials(pid)
    }

    private object UnstableArgTp {
      // existential quantifier and type of corresponding actual arg with unstable type
      def unapply(param: Symbol): Option[(Symbol, Type)] = (params indexOf param) match {
        case -1  => None
        case pid =>
          val sym = existentialFor(pid)
          Some((sym, sym.tpe_*)) // refers to an actual value, must be kind-*
      }
    }

    private object StabilizedArgTp {
      def unapply(param: Symbol): Option[Type] =
        param match {
          case StableArgTp(tp)      => Some(tp)  // (1)
          case UnstableArgTp(_, tp) => Some(tp)  // (2)
          case _ => None
        }
    }

    /** instantiate `param.type` to the (sound approximation of the) type `T`
     * of the actual argument `arg` that was passed in for `param`
     *
     * (1) If `T` is stable, we can just use that.
     *
     * (2) SI-3873: it'd be unsound to instantiate `param.type` to an unstable `T`,
     * so we approximate to `X forSome {type X <: T with Singleton}` -- we can't soundly say more.
     */
    def apply(tp: Type): Type = tp match {
      case SingleType(NoPrefix, StabilizedArgTp(tp)) => tp
      case _                                         => mapOver(tp)
    }

    //AM propagate more info to annotations -- this seems a bit ad-hoc... (based on code by spoon)
    override def mapOver(arg: Tree, giveup: ()=>Nothing): Tree = {
      // TODO: this should be simplified; in the stable case, one can
      // probably just use an Ident to the tree.symbol.
      //
      // @PP: That leads to failure here, where stuff no longer has type
      // 'String @Annot("stuff")' but 'String @Annot(x)'.
      //
      //   def m(x: String): String @Annot(x) = x
      //   val stuff = m("stuff")
      //
      // (TODO cont.) Why an existential in the non-stable case?
      //
      // @PP: In the following:
      //
      //   def m = { val x = "three" ; val y: String @Annot(x) = x; y }
      //
      // m is typed as 'String @Annot(x) forSome { val x: String }'.
      //
      // Both examples are from run/constrained-types.scala.
      object treeTrans extends Transformer {
        override def transform(tree: Tree): Tree = tree.symbol match {
          case StableArgTp(tp)          => gen.mkAttributedQualifier(tp, tree.symbol)
          case UnstableArgTp(quant, tp) => Ident(quant) copyAttrs tree setType tp
          case _                        => super.transform(tree)
        }
      }
      treeTrans transform arg
    }
  }

  /** A map to convert every occurrence of a wildcard type to a fresh
    *  type variable */
  object wildcardToTypeVarMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case WildcardType =>
        TypeVar(tp, new TypeConstraint)
      case BoundedWildcardType(bounds) =>
        TypeVar(tp, new TypeConstraint(bounds))
      case _ =>
        mapOver(tp)
    }
  }

  /** A map to convert each occurrence of a type variable to its origin. */
  object typeVarToOriginMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeVar(origin, _) => origin
      case _ => mapOver(tp)
    }
  }

  /** A map to implement the `contains` method. */
  class ContainsCollector(sym: Symbol) extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        tp match {
          case _: ExistentialType =>
            // ExistentialType#normalize internally calls contains, which leads to exponential performance
            // for types like: `A[_ <: B[_ <: ... ]]`. Example: pos/existential-contains.scala.
            //
            // We can just map over the components and wait until we see the underlying type before we call
            // normalize.
            mapOver(tp)
          case _ =>
            tp.normalize match {
              case TypeRef(_, sym1, _) if (sym == sym1) => result = true
              case refined: RefinedType =>
                mapOver(tp.prefix)
                mapOver(refined)
              case SingleType(_, sym1) if (sym == sym1) => result = true
              case _ => mapOver(tp)
            }
        }
      }
    }

    override def mapOver(arg: Tree) = {
      for (t <- arg) {
        traverse(t.tpe)
        if (t.symbol == sym)
          result = true
      }
      arg
    }
  }

  /** A map to implement the `filter` method. */
  class FilterTypeCollector(p: Type => Boolean) extends TypeCollector[List[Type]](Nil) {
    override def collect(tp: Type) = super.collect(tp).reverse

    def traverse(tp: Type) {
      if (p(tp)) result ::= tp
      mapOver(tp)
    }
  }

  /** A map to implement the `collect` method. */
  class CollectTypeCollector[T](pf: PartialFunction[Type, T]) extends TypeCollector[List[T]](Nil) {
    override def collect(tp: Type) = super.collect(tp).reverse

    def traverse(tp: Type) {
      if (pf.isDefinedAt(tp)) result ::= pf(tp)
      mapOver(tp)
    }
  }

  class ForEachTypeTraverser(f: Type => Unit) extends TypeTraverser {
    def traverse(tp: Type) {
      f(tp)
      mapOver(tp)
    }
  }

  /** A map to implement the `filter` method. */
  class FindTypeCollector(p: Type => Boolean) extends TypeCollector[Option[Type]](None) {
    def traverse(tp: Type) {
      if (result.isEmpty) {
        if (p(tp)) result = Some(tp)
        mapOver(tp)
      }
    }
  }

  /** A map to implement the `contains` method. */
  object ErroneousCollector extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        result = tp.isError
        mapOver(tp)
      }
    }
  }

  object adaptToNewRunMap extends TypeMap {

    private def adaptToNewRun(pre: Type, sym: Symbol): Symbol = {
      if (phase.flatClasses || sym.isRootSymbol || (pre eq NoPrefix) || (pre eq NoType) || sym.isPackageClass)
        sym
      else if (sym.isModuleClass) {
        val sourceModule1 = adaptToNewRun(pre, sym.sourceModule)

        sourceModule1.moduleClass orElse sourceModule1.initialize.moduleClass orElse {
          val msg = "Cannot adapt module class; sym = %s, sourceModule = %s, sourceModule.moduleClass = %s => sourceModule1 = %s, sourceModule1.moduleClass = %s"
          debuglog(msg.format(sym, sym.sourceModule, sym.sourceModule.moduleClass, sourceModule1, sourceModule1.moduleClass))
          sym
        }
      }
      else {
        var rebind0 = pre.findMember(sym.name, BRIDGE, 0, stableOnly = true) orElse {
          if (sym.isAliasType) throw missingAliasException
          devWarning(s"$pre.$sym no longer exist at phase $phase")
          throw new MissingTypeControl // For build manager and presentation compiler purposes
        }
        /* The two symbols have the same fully qualified name */
        def corresponds(sym1: Symbol, sym2: Symbol): Boolean =
          sym1.name == sym2.name && (sym1.isPackageClass || corresponds(sym1.owner, sym2.owner))
        if (!corresponds(sym.owner, rebind0.owner)) {
          debuglog("ADAPT1 pre = "+pre+", sym = "+sym.fullLocationString+", rebind = "+rebind0.fullLocationString)
          val bcs = pre.baseClasses.dropWhile(bc => !corresponds(bc, sym.owner))
          if (bcs.isEmpty)
            assert(pre.typeSymbol.isRefinementClass, pre) // if pre is a refinementclass it might be a structural type => OK to leave it in.
          else
            rebind0 = pre.baseType(bcs.head).member(sym.name)
          debuglog(
            "ADAPT2 pre = " + pre +
              ", bcs.head = " + bcs.head +
              ", sym = " + sym.fullLocationString +
              ", rebind = " + rebind0.fullLocationString
          )
        }
        rebind0.suchThat(sym => sym.isType || sym.isStable) orElse {
          debuglog("" + phase + " " +phase.flatClasses+sym.owner+sym.name+" "+sym.isType)
          throw new MalformedType(pre, sym.nameString)
        }
      }
    }
    def apply(tp: Type): Type = tp match {
      case ThisType(sym) =>
        try {
          val sym1 = adaptToNewRun(sym.owner.thisType, sym)
          if (sym1 == sym) tp else ThisType(sym1)
        } catch {
          case ex: MissingTypeControl =>
            tp
        }
      case SingleType(pre, sym) =>
        if (sym.hasPackageFlag) tp
        else {
          val pre1 = this(pre)
          try {
            val sym1 = adaptToNewRun(pre1, sym)
            if ((pre1 eq pre) && (sym1 eq sym)) tp
            else singleType(pre1, sym1)
          } catch {
            case _: MissingTypeControl =>
              tp
          }
        }
      case TypeRef(pre, sym, args) =>
        if (sym.isPackageClass) tp
        else {
          val pre1 = this(pre)
          val args1 = args mapConserve (this)
          try {
            val sym1 = adaptToNewRun(pre1, sym)
            if ((pre1 eq pre) && (sym1 eq sym) && (args1 eq args)/* && sym.isExternal*/) {
              tp
            } else if (sym1 == NoSymbol) {
              devWarning(s"adapt to new run failed: pre=$pre pre1=$pre1 sym=$sym")
              tp
            } else {
              copyTypeRef(tp, pre1, sym1, args1)
            }
          } catch {
            case ex: MissingAliasControl =>
              apply(tp.dealias)
            case _: MissingTypeControl =>
              tp
          }
        }
      case MethodType(params, restp) =>
        val restp1 = this(restp)
        if (restp1 eq restp) tp
        else copyMethodType(tp, params, restp1)
      case NullaryMethodType(restp) =>
        val restp1 = this(restp)
        if (restp1 eq restp) tp
        else NullaryMethodType(restp1)
      case PolyType(tparams, restp) =>
        val restp1 = this(restp)
        if (restp1 eq restp) tp
        else PolyType(tparams, restp1)

      // Lukas: we need to check (together) whether we should also include parameter types
      // of PolyType and MethodType in adaptToNewRun

      case ClassInfoType(parents, decls, clazz) =>
        if (clazz.isPackageClass) tp
        else {
          val parents1 = parents mapConserve (this)
          if (parents1 eq parents) tp
          else ClassInfoType(parents1, decls, clazz)
        }
      case RefinedType(parents, decls) =>
        val parents1 = parents mapConserve (this)
        if (parents1 eq parents) tp
        else refinedType(parents1, tp.typeSymbol.owner, decls, tp.typeSymbol.owner.pos)
      case SuperType(_, _) => mapOver(tp)
      case TypeBounds(_, _) => mapOver(tp)
      case TypeVar(_, _) => mapOver(tp)
      case AnnotatedType(_, _) => mapOver(tp)
      case ExistentialType(_, _) => mapOver(tp)
      case _ => tp
    }
  }

}
