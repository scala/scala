/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import Variance._
import scala.collection.{ mutable, immutable }

/** See comments at scala.reflect.internal.Variance.
 */
trait Variances {
  self: SymbolTable =>

  /** Used by ExistentialExtrapolation and adaptConstrPattern().
   *  TODO - eliminate duplication with all the rest.
   */
  trait VariantTypeMap extends TypeMap {
    private[this] var _variance: Variance = Covariant

    override def variance = _variance
    def variance_=(x: Variance) = _variance = x

    override protected def noChangeToSymbols(origSyms: List[Symbol]) =
      //OPT inline from forall to save on #closures
      origSyms match {
        case sym :: rest =>
          val v = variance
          if (sym.isAliasType) variance = Invariant
          val result = this(sym.info)
          variance = v
          (result eq sym.info) && noChangeToSymbols(rest)
        case _ =>
          true
      }

    override protected def mapOverArgs(args: List[Type], tparams: List[Symbol]): List[Type] =
      map2Conserve(args, tparams) { (arg, tparam) =>
        val v = variance
        if (tparam.isContravariant) variance = variance.flip
        else if (!tparam.isCovariant) variance = Invariant
        val arg1 = this(arg)
        variance = v
        arg1
      }

    /** Map this function over given type */
    override def mapOver(tp: Type): Type = tp match {
      case MethodType(params, result) =>
        variance = variance.flip
        val params1 = mapOver(params)
        variance = variance.flip
        val result1 = this(result)
        if ((params1 eq params) && (result1 eq result)) tp
        else copyMethodType(tp, params1, result1.substSym(params, params1))
      case PolyType(tparams, result) =>
        variance = variance.flip
        val tparams1 = mapOver(tparams)
        variance = variance.flip
        val result1 = this(result)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else PolyType(tparams1, result1.substSym(tparams, tparams1))
      case TypeBounds(lo, hi) =>
        variance = variance.flip
        val lo1 = this(lo)
        variance = variance.flip
        val hi1 = this(hi)
        if ((lo1 eq lo) && (hi1 eq hi)) tp
        else TypeBounds(lo1, hi1)
      case tr @ TypeRef(pre, sym, args) =>
        val pre1 = this(pre)
        val args1 =
          if (args.isEmpty)
            args
          else if (variance.isInvariant) // fast & safe path: don't need to look at typeparams
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

  /** Used in Refchecks.
   *  TODO - eliminate duplication with varianceInType
   */
  class VarianceValidator extends Traverser {
    val escapedPrivateLocals = mutable.HashSet[Symbol]()

    protected def issueVarianceError(base: Symbol, sym: Symbol, required: Variance): Unit = ()

    /** Validate variance of info of symbol `base` */
    private def validateVariance(base: Symbol) {
      // A flag for when we're in a refinement, meaning method parameter types
      // need to be checked.
      var inRefinement = false

      /** The variance of a symbol occurrence of `tvar`
       *  seen at the level of the definition of `base`.
       *  The search proceeds from `base` to the owner of `tvar`.
       *  Initially the state is covariant, but it might change along the search.
       */
      def relativeVariance(tvar: Symbol): Variance = {
        val clazz = tvar.owner
        var sym = base
        var state: Variance = Covariant
        while (sym != clazz && !state.isBivariant) {
          //Console.println("flip: " + sym + " " + sym.isParameter());//DEBUG
          // Flip occurrences of type parameters and parameters, unless
          //  - it's a constructor, or case class factory or extractor
          //  - it's a type parameter of tvar's owner.
          if (sym.isParameter && !sym.owner.isConstructor && !sym.owner.isCaseApplyOrUnapply &&
              !(tvar.isTypeParameterOrSkolem && sym.isTypeParameterOrSkolem &&
                tvar.owner == sym.owner)) state = state.flip
          else if (!sym.owner.isClass ||
                   sym.isTerm && ((sym.isPrivateLocal || sym.isProtectedLocal || sym.isSuperAccessor /* super accessors are implicitly local #4345*/) && !(escapedPrivateLocals contains sym))) {
            // return Bivariant if `sym` is local to a term
            // or is private[this] or protected[this]
            state = Bivariant
          }
          else if (sym.isAliasType) {
            // return Bivariant if `sym` is an alias type
            // that does not override anything. This is OK, because we always
            // expand aliases for variance checking.
            // However, if `sym` does override a type in a base class
            // we have to assume Invariant, as there might then be
            // references to the type parameter that are not variance checked.
            state = if (sym.isOverridingSymbol) Invariant else Bivariant
          }
          sym = sym.owner
        }
        state
      }

      /** Validate that the type `tp` is variance-correct, assuming
       *  the type occurs itself at variance position given by `variance`
       */
      def validateVariance(tp: Type, variance: Variance): Unit = tp match {
        case ErrorType =>
        case WildcardType =>
        case BoundedWildcardType(bounds) =>
          validateVariance(bounds, variance)
        case NoType =>
        case NoPrefix =>
        case ThisType(_) =>
        case ConstantType(_) =>
        case SingleType(pre, sym) =>
          validateVariance(pre, variance)
        case TypeRef(pre, sym, args) =>
//            println("validate "+sym+" at "+relativeVariance(sym))
          if (sym.isAliasType/* && relativeVariance(sym) == Bivariant*/)
            validateVariance(tp.normalize, variance)
          else if (!sym.variance.isInvariant) {
            val v = relativeVariance(sym)
            val requiredVariance = v * variance

            if (!v.isBivariant && sym.variance != requiredVariance)
              issueVarianceError(base, sym, requiredVariance)
          }
          validateVariance(pre, variance)
          // @M for higher-kinded typeref, args.isEmpty
          // However, these args respect variances by construction anyway
          // -- the interesting case is in type application, see checkKindBounds in Infer
          if (args.nonEmpty)
            validateVarianceArgs(args, variance, sym.typeParams)
        case ClassInfoType(parents, decls, symbol) =>
          validateVariances(parents, variance)
        case RefinedType(parents, decls) =>
          validateVariances(parents, variance)
          val saved = inRefinement
          inRefinement = true
          for (sym <- decls)
            validateVariance(sym.info, if (sym.isAliasType) Invariant else variance)
          inRefinement = saved
        case TypeBounds(lo, hi) =>
          validateVariance(lo, variance.flip)
          validateVariance(hi, variance)
        case mt @ MethodType(formals, result) =>
          if (inRefinement)
            validateVariances(mt.paramTypes, variance.flip)
          validateVariance(result, variance)
        case NullaryMethodType(result) =>
          validateVariance(result, variance)
        case PolyType(tparams, result) =>
          // type parameters will be validated separately, because they are defined explicitly.
          validateVariance(result, variance)
        case ExistentialType(tparams, result) =>
          validateVariances(tparams map (_.info), variance)
          validateVariance(result, variance)
        case AnnotatedType(annots, tp, selfsym) =>
          if (!annots.exists(_ matches definitions.uncheckedVarianceClass))
            validateVariance(tp, variance)
      }

      def validateVariances(tps: List[Type], variance: Variance) {
        tps foreach (tp => validateVariance(tp, variance))
      }

      def validateVarianceArgs(tps: List[Type], variance: Variance, tparams: List[Symbol]) {
        foreach2(tps, tparams)((tp, tparam) => validateVariance(tp, variance * tparam.variance))
      }

      validateVariance(base.info, Covariant)
    }

    override def traverse(tree: Tree) {
      tree match {
        case ClassDef(_, _, _, _) | TypeDef(_, _, _, _) =>
          validateVariance(tree.symbol)
          super.traverse(tree)
        // ModuleDefs need not be considered because they have been eliminated already
        case ValDef(_, _, _, _) =>
          if (!tree.symbol.hasLocalFlag)
            validateVariance(tree.symbol)
        case DefDef(_, _, tparams, vparamss, _, _) =>
          // No variance check for object-private/protected methods/values.
          if (!tree.symbol.hasLocalFlag) {
            validateVariance(tree.symbol)
            traverseTrees(tparams)
            traverseTreess(vparamss)
          }
        case Template(_, _, _) =>
          super.traverse(tree)
        case _ =>
      }
    }
  }

  /** Compute variance of type parameter `tparam` in all types `tps`. */
  def varianceInTypes(tps: List[Type])(tparam: Symbol): Variance =
    fold(tps map (tp => varianceInType(tp)(tparam)))

  /** Compute variance of type parameter `tparam` in type `tp`. */
  def varianceInType(tp: Type)(tparam: Symbol): Variance = {
    def inArgs(sym: Symbol, args: List[Type]): Variance = fold(map2(args, sym.typeParams)((a, p) => inType(a) * p.variance))
    def inSyms(syms: List[Symbol]): Variance            = fold(syms map inSym)
    def inTypes(tps: List[Type]): Variance              = fold(tps map inType)

    def inSym(sym: Symbol): Variance = if (sym.isAliasType) inType(sym.info).cut else inType(sym.info)
    def inType(tp: Type): Variance   = tp match {
      case ErrorType | WildcardType | NoType | NoPrefix => Bivariant
      case ThisType(_) | ConstantType(_)                => Bivariant
      case TypeRef(_, `tparam`, _)                      => Covariant
      case BoundedWildcardType(bounds)                  => inType(bounds)
      case NullaryMethodType(restpe)                    => inType(restpe)
      case SingleType(pre, sym)                         => inType(pre)
      case TypeRef(pre, _, _) if tp.isHigherKinded      => inType(pre)                 // a type constructor cannot occur in tp's args
      case TypeRef(pre, sym, args)                      => inType(pre)                 & inArgs(sym, args)
      case TypeBounds(lo, hi)                           => inType(lo).flip             & inType(hi)
      case RefinedType(parents, defs)                   => inTypes(parents)            & inSyms(defs.toList)
      case MethodType(params, restpe)                   => inSyms(params).flip         & inType(restpe)
      case PolyType(tparams, restpe)                    => inSyms(tparams).flip        & inType(restpe)
      case ExistentialType(tparams, restpe)             => inSyms(tparams)             & inType(restpe)
      case AnnotatedType(annots, tp, _)                 => inTypes(annots map (_.atp)) & inType(tp)
    }

    inType(tp)
  }
}
