package scala.reflect
package internal
package tpe

import scala.collection.{ mutable, immutable }
import Flags._
import scala.annotation.tailrec

private[internal] trait TypeMaps {
  self: SymbolTable =>
  import definitions._

  /** Normalize any type aliases within this type (@see Type#normalize).
   *  Note that this depends very much on the call to "normalize", not "dealias",
   *  so it is no longer carries the too-stealthy name "deAlias".
   */
  object normalizeAliases extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(_, sym, _) if sym.isAliasType =>
        def msg = if (tp.isHigherKinded) s"Normalizing type alias function $tp" else s"Dealiasing type alias $tp"
        mapOver(logResult(msg)(tp.normalize))
      case _                                     => mapOver(tp)
    }
  }

  /** Remove any occurrence of type <singleton> from this type and its parents */
  object dropSingletonType extends TypeMap {
    def apply(tp: Type): Type = {
      tp match {
        case TypeRef(_, SingletonClass, _) =>
          AnyClass.tpe
        case tp1 @ RefinedType(parents, decls) =>
          parents filter (_.typeSymbol != SingletonClass) match {
            case Nil                       => AnyClass.tpe
            case p :: Nil if decls.isEmpty => mapOver(p)
            case ps                        => mapOver(copyRefinedType(tp1, ps, decls))
          }
        case tp1 =>
          mapOver(tp1)
      }
    }
  }

  /** Substitutes the empty scope for any non-empty decls in the type. */
  object dropAllRefinements extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case rt @ RefinedType(parents, decls) if !decls.isEmpty =>
        mapOver(copyRefinedType(rt, parents, EmptyScope))
      case ClassInfoType(parents, decls, clazz) if !decls.isEmpty =>
        mapOver(ClassInfoType(parents, EmptyScope, clazz))
      case _ =>
        mapOver(tp)
    }
  }

  /** Type with all top-level occurrences of abstract types replaced by their bounds */
  def abstractTypesToBounds(tp: Type): Type = tp match { // @M don't normalize here (compiler loops on pos/bug1090.scala )
    case TypeRef(_, sym, _) if sym.isAbstractType =>
      abstractTypesToBounds(tp.bounds.hi)
    case TypeRef(_, sym, _) if sym.isAliasType =>
      abstractTypesToBounds(tp.normalize)
    case rtp @ RefinedType(parents, decls) =>
      copyRefinedType(rtp, parents mapConserve abstractTypesToBounds, decls)
    case AnnotatedType(_, underlying, _) =>
      abstractTypesToBounds(underlying)
    case _ =>
      tp
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
  object dropRepeatedParamType extends TypeMap {
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

  object toDeBruijn extends TypeMap {
    private var paramStack: List[List[Symbol]] = Nil
    def mkDebruijnBinder(params: List[Symbol], restpe: Type) = {
      paramStack = params :: paramStack
      try {
        DeBruijnBinder(params map (_.name), params map (p => this(p.info)), this(restpe))
      } finally paramStack = paramStack.tail
    }
    def apply(tp: Type): Type = tp match {
      case PolyType(tparams, restpe) =>
        mkDebruijnBinder(tparams, restpe)
      case MethodType(params, restpe) =>
        mkDebruijnBinder(params, restpe)
      case TypeRef(NoPrefix, sym, args) =>
        val level = paramStack indexWhere (_ contains sym)
        if (level < 0) mapOver(tp)
        else DeBruijnIndex(level, paramStack(level) indexOf sym, args mapConserve this)
      case _ =>
        mapOver(tp)
    }
  }

  def fromDeBruijn(owner: Symbol) = new TypeMap {
    private var paramStack: List[List[Symbol]] = Nil
    def apply(tp: Type): Type = tp match {
      case DeBruijnBinder(pnames, ptypes, restpe) =>
        val isType = pnames.head.isTypeName
        val newParams = for (name <- pnames) yield
          if (isType) owner.newTypeParameter(name.toTypeName)
          else owner.newValueParameter(name.toTermName)
        paramStack = newParams :: paramStack
        try {
          foreach2(newParams, ptypes)((p, t) => p setInfo this(t))
          val restpe1 = this(restpe)
          if (isType) PolyType(newParams, restpe1)
          else MethodType(newParams, restpe1)
        } finally paramStack = paramStack.tail
      case DeBruijnIndex(level, idx, args) =>
        TypeRef(NoPrefix, paramStack(level)(idx), args map this)
      case _ =>
        mapOver(tp)
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

  trait VariantTypeMap extends TypeMap {
    private[this] var _variance = 1

    override def variance = _variance
    def variance_=(x: Int) = _variance = x

    override protected def noChangeToSymbols(origSyms: List[Symbol]) =
      //OPT inline from forall to save on #closures
      origSyms match {
        case sym :: rest =>
          val v = variance
          if (sym.isAliasType) variance = 0
          val result = this(sym.info)
          variance = v
          (result eq sym.info) && noChangeToSymbols(rest)
        case _ =>
          true
      }

    override protected def mapOverArgs(args: List[Type], tparams: List[Symbol]): List[Type] =
      map2Conserve(args, tparams) { (arg, tparam) =>
        val v = variance
        if (tparam.isContravariant) variance = -variance
        else if (!tparam.isCovariant) variance = 0
        val arg1 = this(arg)
        variance = v
        arg1
      }

    /** Map this function over given type */
    override def mapOver(tp: Type): Type = tp match {
      case MethodType(params, result) =>
        variance = -variance
        val params1 = mapOver(params)
        variance = -variance
        val result1 = this(result)
        if ((params1 eq params) && (result1 eq result)) tp
        else copyMethodType(tp, params1, result1.substSym(params, params1))
      case PolyType(tparams, result) =>
        variance = -variance
        val tparams1 = mapOver(tparams)
        variance = -variance
        var result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else PolyType(tparams1, result1.substSym(tparams, tparams1))
      case TypeBounds(lo, hi) =>
        variance = -variance
        val lo1 = this(lo)
        variance = -variance
        val hi1 = this(hi)
        if ((lo1 eq lo) && (hi1 eq hi)) tp
        else TypeBounds(lo1, hi1)
      case tr @ TypeRef(pre, sym, args) =>
        val pre1 = this(pre)
        val args1 =
          if (args.isEmpty)
            args
          else if (variance == 0) // fast & safe path: don't need to look at typeparams
            args mapConserve this
          else {
            val tparams = sym.typeParams
            if (tparams.isEmpty) args
            else mapOverArgs(args, tparams)
          }
        if ((pre1 eq pre) && (args1 eq args)) tp
        else copyTypeRef(tp, pre1, tr.coevolveSym(pre1), args1)
      case _ =>
        super.mapOver(tp)
    }
  }

  // todo. move these into scala.reflect.api

  /** A prototype for mapping a function over all possible types
   */
  abstract class TypeMap extends (Type => Type) {
    def apply(tp: Type): Type

    /** Mix in VariantTypeMap if you want variances to be significant.
     */
    def variance = 0

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case tr @ TypeRef(pre, sym, args) =>
        val pre1 = this(pre)
        val args1 = args mapConserve this
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
        val params1 = mapOver(params)
        val result1 = this(result)
        if ((params1 eq params) && (result1 eq result)) tp
        else copyMethodType(tp, params1, result1.substSym(params, params1))
      case PolyType(tparams, result) =>
        val tparams1 = mapOver(tparams)
        var result1 = this(result)
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
        val lo1 = this(lo)
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
        //if ((parents1 eq parents) && (decls1 eq decls)) tp
        //else refinementOfClass(tp.typeSymbol, parents1, decls1)
        copyRefinedType(rtp, parents1, decls1)
      case ExistentialType(tparams, result) =>
        val tparams1 = mapOver(tparams)
        var result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else newExistentialType(tparams1, result1.substSym(tparams, tparams1))
      case OverloadedType(pre, alts) =>
        val pre1 = if (pre.isInstanceOf[ClassInfoType]) pre else this(pre)
        if (pre1 eq pre) tp
        else OverloadedType(pre1, alts)
      case AntiPolyType(pre, args) =>
        val pre1 = this(pre)
        val args1 = args mapConserve (this)
        if ((pre1 eq pre) && (args1 eq args)) tp
        else AntiPolyType(pre1, args1)
      case tv@TypeVar(_, constr) =>
        if (constr.instValid) this(constr.inst)
        else tv.applyArgs(mapOverArgs(tv.typeArgs, tv.params))  //@M !args.isEmpty implies !typeParams.isEmpty
      case NotNullType(tp) =>
        val tp1 = this(tp)
        if (tp1 eq tp) tp
        else NotNullType(tp1)
      case AnnotatedType(annots, atp, selfsym) =>
        val annots1 = mapOverAnnotations(annots)
        val atp1 = this(atp)
        if ((annots1 eq annots) && (atp1 eq atp)) tp
        else if (annots1.isEmpty) atp1
        else AnnotatedType(annots1, atp1, selfsym)
      case DeBruijnIndex(shift, idx, args) =>
        val args1 = args mapConserve this
        if (args1 eq args) tp
        else DeBruijnIndex(shift, idx, args1)
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

    protected def mapOverArgs(args: List[Type], tparams: List[Symbol]): List[Type] =
      args mapConserve this

    /** Called by mapOver to determine whether the original symbols can
     *  be returned, or whether they must be cloned.  Overridden in VariantTypeMap.
     */
    protected def noChangeToSymbols(origSyms: List[Symbol]) =
      origSyms forall (sym => sym.info eq this(sym.info))

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

  /** A collector that tests for existential types appearing at given variance in a type
   *  @PP: Commenting out due to not being used anywhere.
   */
  // class ContainsVariantExistentialCollector(v: Int) extends TypeCollector(false) with VariantTypeMap {
  //   variance = v
  //
  //   def traverse(tp: Type) = tp match {
  //     case ExistentialType(_, _) if (variance == v) => result = true
  //     case _ => mapOver(tp)
  //   }
  // }
  //
  // val containsCovariantExistentialCollector = new ContainsVariantExistentialCollector(1)
  // val containsContravariantExistentialCollector = new ContainsVariantExistentialCollector(-1)

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
        if (expanded contains sym) AnyRefClass.tpe
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

  /** Used by existentialAbstraction.
   */
  class ExistentialExtrapolation(tparams: List[Symbol]) extends VariantTypeMap {
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

    def apply(tp: Type): Type = {
      val tp1 = mapOver(tp)
      if (variance == 0) tp1
      else tp1 match {
        case TypeRef(pre, sym, args) if tparams contains sym =>
          val repl = if (variance == 1) dropSingletonType(tp1.bounds.hi) else tp1.bounds.lo
          //println("eliminate "+sym+"/"+repl+"/"+occurCount(sym)+"/"+(tparams exists (repl.contains)))//DEBUG
          if (!repl.typeSymbol.isBottomClass && occurCount(sym) == 1 && !(tparams exists (repl.contains)))
            repl
          else tp1
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

  /** Might the given symbol be important when calculating the prefix
   *  of a type? When tp.asSeenFrom(pre, clazz) is called on `tp`,
   *  the result will be `tp` unchanged if `pre` is trivial and `clazz`
   *  is a symbol such that isPossiblePrefix(clazz) == false.
   */
  def isPossiblePrefix(clazz: Symbol) = clazz.isClass && !clazz.isPackageClass

  protected[internal] def skipPrefixOf(pre: Type, clazz: Symbol) = (
    (pre eq NoType) || (pre eq NoPrefix) || !isPossiblePrefix(clazz)
  )

  /** A map to compute the asSeenFrom method  */
  class AsSeenFromMap(pre: Type, clazz: Symbol) extends TypeMap with KeepOnlyTypeConstraints {
    var capturedSkolems: List[Symbol] = List()
    var capturedParams: List[Symbol] = List()

    override def mapOver(tree: Tree, giveup: ()=>Nothing): Tree = {
      object annotationArgRewriter extends TypeMapTransformer {
        private def canRewriteThis(sym: Symbol) = (
             (sym isNonBottomSubClass clazz)
          && (pre.widen.typeSymbol isNonBottomSubClass sym)
          && (pre.isStable || giveup())
        )
        // what symbol should really be used?
        private def newTermSym() = {
          val p = pre.typeSymbol
          p.owner.newValue(p.name.toTermName, p.pos) setInfo pre
        }
        /** Rewrite `This` trees in annotation argument trees */
        override def transform(tree: Tree): Tree = super.transform(tree) match {
          case This(_) if canRewriteThis(tree.symbol) => gen.mkAttributedQualifier(pre, newTermSym())
          case tree                                   => tree
        }
      }
      annotationArgRewriter.transform(tree)
    }

    def stabilize(pre: Type, clazz: Symbol): Type = {
      capturedParams find (_.owner == clazz) match {
        case Some(qvar) => qvar.tpe
        case _          =>
          val qvar = clazz freshExistential nme.SINGLETON_SUFFIX setInfo singletonBounds(pre)
          capturedParams ::= qvar
          qvar.tpe
      }
    }

    def apply(tp: Type): Type =
      tp match {
        case ThisType(sym) =>
          def toPrefix(pre: Type, clazz: Symbol): Type =
            if (skipPrefixOf(pre, clazz)) tp
            else if ((sym isNonBottomSubClass clazz) &&
                     (pre.widen.typeSymbol isNonBottomSubClass sym)) {
              val pre1 = pre match {
                case SuperType(thistp, _) => thistp
                case _ => pre
              }
              if (!(pre1.isStable ||
                    pre1.typeSymbol.isPackageClass ||
                    pre1.typeSymbol.isModuleClass && pre1.typeSymbol.isStatic)) {
                stabilize(pre1, sym)
              } else {
                pre1
              }
            } else {
              toPrefix(pre.baseType(clazz).prefix, clazz.owner)
            }
          toPrefix(pre, clazz)
        case SingleType(pre, sym) =>
          if (sym.isPackageClass) tp // short path
          else {
            val pre1 = this(pre)
            if (pre1 eq pre) tp
            else if (pre1.isStable) singleType(pre1, sym)
            else pre1.memberType(sym).resultType //todo: this should be rolled into existential abstraction
          }
        // AM: Martin, is this description accurate?
        // walk the owner chain of `clazz` (the original argument to asSeenFrom) until we find the type param's owner (while rewriting pre as we crawl up the owner chain)
        // once we're at the owner, extract the information that pre encodes about the type param,
        // by minimally subsuming pre to the type instance of the class that owns the type param,
        // the type we're looking for is the type instance's type argument at the position corresponding to the type parameter
        // optimisation: skip this type parameter if it's not owned by a class, as those params are not influenced by the prefix through which they are seen
        // (concretely: type params of anonymous type functions, which currently can only arise from normalising type aliases, are owned by the type alias of which they are the eta-expansion)
        // (skolems also aren't affected: they are ruled out by the isTypeParameter check)
        case TypeRef(prefix, sym, args) if (sym.isTypeParameter && sym.owner.isClass) =>
          def toInstance(pre: Type, clazz: Symbol): Type =
            if (skipPrefixOf(pre, clazz)) mapOver(tp)
            //@M! see test pos/tcpoly_return_overriding.scala why mapOver is necessary
            else {
              def throwError = abort("" + tp + sym.locationString + " cannot be instantiated from " + pre.widen)

              val symclazz = sym.owner
              if (symclazz == clazz && !pre.widen.isInstanceOf[TypeVar] && (pre.widen.typeSymbol isNonBottomSubClass symclazz)) {
                // have to deconst because it may be a Class[T].
                pre.baseType(symclazz).deconst match {
                  case TypeRef(_, basesym, baseargs) =>

                   def instParam(ps: List[Symbol], as: List[Type]): Type =
                      if (ps.isEmpty) {
                        if (forInteractive) {
                          val saved = settings.uniqid.value
                          try {
                            settings.uniqid.value = true
                            println("*** stale type parameter: " + tp + sym.locationString + " cannot be instantiated from " + pre.widen)
                            println("*** confused with params: " + sym + " in " + sym.owner + " not in " + ps + " of " + basesym)
                            println("*** stacktrace = ")
                            new Error().printStackTrace()
                          } finally settings.uniqid.value = saved
                          instParamRelaxed(basesym.typeParams, baseargs)
                        } else throwError
                      } else if (sym eq ps.head)
                        // @M! don't just replace the whole thing, might be followed by type application
                        appliedType(as.head, args mapConserve (this)) // @M: was as.head
                      else instParam(ps.tail, as.tail)

                    /** Relaxed version of instParams which matches on names not symbols.
                     *  This is a last fallback in interactive mode because races in calls
                     *  from the IDE to the compiler may in rare cases lead to symbols referring
                     *  to type parameters that are no longer current.
                     */
                    def instParamRelaxed(ps: List[Symbol], as: List[Type]): Type =
                      if (ps.isEmpty) throwError
                      else if (sym.name == ps.head.name)
                        // @M! don't just replace the whole thing, might be followed by type application
                        appliedType(as.head, args mapConserve (this)) // @M: was as.head
                      else instParamRelaxed(ps.tail, as.tail)

                    //Console.println("instantiating " + sym + " from " + basesym + " with " + basesym.typeParams + " and " + baseargs+", pre = "+pre+", symclazz = "+symclazz);//DEBUG
                    if (sameLength(basesym.typeParams, baseargs))
                      instParam(basesym.typeParams, baseargs)
                    else
                      if (symclazz.tpe.parents exists typeIsErroneous)
                        ErrorType // don't be to overzealous with throwing exceptions, see #2641
                      else
                        throw new Error(
                          "something is wrong (wrong class file?): "+basesym+
                          " with type parameters "+
                          basesym.typeParams.map(_.name).mkString("[",",","]")+
                          " gets applied to arguments "+baseargs.mkString("[",",","]")+", phase = "+phase)
                  case ExistentialType(tparams, qtpe) =>
                    capturedSkolems = capturedSkolems union tparams
                    toInstance(qtpe, clazz)
                  case t =>
                    throwError
                }
              } else toInstance(pre.baseType(clazz).prefix, clazz.owner)
            }
          toInstance(pre, clazz)
        case _ =>
          mapOver(tp)
      }
  }

  /** A base class to compute all substitutions */
  abstract class SubstMap[T](from: List[Symbol], to: List[T]) extends TypeMap {
    assert(sameLength(from, to), "Unsound substitution from "+ from +" to "+ to)

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

    def apply(tp0: Type): Type = if (from.isEmpty) tp0 else {
      @tailrec def subst(tp: Type, sym: Symbol, from: List[Symbol], to: List[T]): Type =
        if (from.isEmpty) tp
        // else if (to.isEmpty) error("Unexpected substitution on '%s': from = %s but to == Nil".format(tp, from))
        else if (matches(from.head, sym)) toType(tp, to.head)
        else subst(tp, sym, from.tail, to.tail)

      val boundSyms = tp0.boundSyms
      val tp1 = if (boundSyms.nonEmpty && (boundSyms exists from.contains)) renameBoundSyms(tp0) else tp0
      val tp = mapOver(tp1)

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
          appliedType(subst(tp, sym, from, to), args) // if args.isEmpty, appliedType is the identity
        case SingleType(NoPrefix, sym) =>
          subst(tp, sym, from, to)
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
    override def apply(tp: Type): Type = if (from.isEmpty) tp else {
      @tailrec def subst(sym: Symbol, from: List[Symbol], to: List[Symbol]): Symbol =
        if (from.isEmpty) sym
        // else if (to.isEmpty) error("Unexpected substitution on '%s': from = %s but to == Nil".format(sym, from))
        else if (matches(from.head, sym)) to.head
        else subst(sym, from.tail, to.tail)
      tp match {
        case TypeRef(pre, sym, args) if pre ne NoPrefix =>
          val newSym = subst(sym, from, to)
          // mapOver takes care of subst'ing in args
          mapOver ( if (sym eq newSym) tp else copyTypeRef(tp, pre, newSym, args) )
          // assert(newSym.typeParams.length == sym.typeParams.length, "typars mismatch in SubstSymMap: "+(sym, sym.typeParams, newSym, newSym.typeParams))
        case SingleType(pre, sym) if pre ne NoPrefix =>
          val newSym = subst(sym, from, to)
          mapOver( if (sym eq newSym) tp else singleType(pre, newSym) )
        case _ =>
          super.apply(tp)
      }
    }

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
  class SubstTypeMap(from: List[Symbol], to: List[Type])
  extends SubstMap(from, to) {
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
      else if (!result) mapOver(tp)
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
    def existentialsNeeded: List[Symbol] = existentials.filter(_ ne null).toList

    private object StableArg {
      def unapply(param: Symbol) = Arg unapply param map actuals filter (tp =>
        tp.isStable && (tp.typeSymbol != NothingClass)
      )
    }
    private object Arg {
      def unapply(param: Symbol) = Some(params indexOf param) filter (_ >= 0)
    }

    def apply(tp: Type): Type = mapOver(tp) match {
      // unsound to replace args by unstable actual #3873
      case SingleType(NoPrefix, StableArg(arg)) => arg
      // (soundly) expand type alias selections on implicit arguments,
      // see depmet_implicit_oopsla* test cases -- typically, `param.isImplicit`
      case tp1 @ TypeRef(SingleType(NoPrefix, Arg(pid)), sym, targs) =>
        val arg = actuals(pid)
        val res = typeRef(arg, sym, targs)
        if (res.typeSymbolDirect.isAliasType) res.dealias else tp1
      // don't return the original `tp`, which may be different from `tp1`,
      // due to dropping annotations
      case tp1 => tp1
    }

    /* Return the type symbol for referencing a parameter inside the existential quantifier.
     * (Only needed if the actual is unstable.)
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
          case StableArg(actual) =>
            gen.mkAttributedQualifier(actual, tree.symbol)
          case Arg(pid) =>
            val sym = existentialFor(pid)
            Ident(sym) copyAttrs tree setType typeRef(NoPrefix, sym, Nil)
          case _ =>
            super.transform(tree)
        }
      }
      treeTrans transform arg
    }
  }

  object StripAnnotationsMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case AnnotatedType(_, atp, _) =>
        mapOver(atp)
      case tp =>
        mapOver(tp)
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

  /** A map to convert every occurrence of a type variable to a wildcard type. */
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
        tp.normalize match {
          case TypeRef(_, sym1, _) if (sym == sym1) => result = true
          case SingleType(_, sym1) if (sym == sym1) => result = true
          case _ => mapOver(tp)
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

  /** A map to implement the `contains` method. */
  class ContainsTypeCollector(t: Type) extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        if (tp eq t) result = true
        else mapOver(tp)
      }
    }
    override def mapOver(arg: Tree) = {
      for (t <- arg)
        traverse(t.tpe)

      arg
    }
  }

  /** A map to implement the `filter` method. */
  class FilterTypeCollector(p: Type => Boolean) extends TypeCollector[List[Type]](Nil) {
    def withFilter(q: Type => Boolean) = new FilterTypeCollector(tp => p(tp) && q(tp))

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
        var rebind0 = pre.findMember(sym.name, BRIDGE, 0, true) orElse {
          if (sym.isAliasType) throw missingAliasException
          debugwarn(pre+"."+sym+" does no longer exist, phase = "+phase)
          throw new MissingTypeControl // For build manager and presentation compiler purposes
        }
        /** The two symbols have the same fully qualified name */
        def corresponds(sym1: Symbol, sym2: Symbol): Boolean =
          sym1.name == sym2.name && (sym1.isPackageClass || corresponds(sym1.owner, sym2.owner))
        if (!corresponds(sym.owner, rebind0.owner)) {
          debuglog("ADAPT1 pre = "+pre+", sym = "+sym.fullLocationString+", rebind = "+rebind0.fullLocationString)
          val bcs = pre.baseClasses.dropWhile(bc => !corresponds(bc, sym.owner));
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
        if (sym.isPackage) tp
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
              debugwarn("adapt fail: "+pre+" "+pre1+" "+sym)
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
      case AnnotatedType(_,_,_) => mapOver(tp)
      case NotNullType(_) => mapOver(tp)
      case ExistentialType(_, _) => mapOver(tp)
      case _ => tp
    }
  }
}
