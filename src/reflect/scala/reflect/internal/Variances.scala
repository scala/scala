/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import Variance._
import scala.collection.{ mutable, immutable }
import scala.annotation.tailrec

/** See comments at scala.reflect.internal.Variance.
 */
trait Variances {
  self: SymbolTable =>

  /** Used in Refchecks.
   *  TODO - eliminate duplication with varianceInType
   */
  class VarianceValidator extends Traverser {
    private val escapedLocals = mutable.HashSet[Symbol]()

    /** Is every symbol in the owner chain between `site` and the owner of `sym`
     *  either a term symbol or private[this]? If not, add `sym` to the set of
     *  esacped locals.
     *  @pre  sym.hasLocalFlag
     */
    @tailrec final def checkForEscape(sym: Symbol, site: Symbol) {
      if (site == sym.owner || site == sym.owner.moduleClass || site.isPackage) () // done
      else if (site.isTerm || site.isPrivateLocal) checkForEscape(sym, site.owner) // ok - recurse to owner
      else escapedLocals += sym
    }

    protected def issueVarianceError(base: Symbol, sym: Symbol, required: Variance): Unit = ()

    // Flip occurrences of type parameters and parameters, unless
    //  - it's a constructor, or case class factory or extractor
    //  - it's a type parameter of tvar's owner.
    def shouldFlip(sym: Symbol, tvar: Symbol) = (
         sym.isParameter
      && !sym.owner.isConstructor
      && !sym.owner.isCaseApplyOrUnapply
      && !(tvar.isTypeParameterOrSkolem && sym.isTypeParameterOrSkolem && tvar.owner == sym.owner)
    )
    // return Bivariant if `sym` is local to a term
    // or is private[this] or protected[this]
    def isLocalOnly(sym: Symbol) = !sym.owner.isClass || (
         sym.isTerm
      && (sym.isPrivateLocal || sym.isProtectedLocal || sym.isSuperAccessor) // super accessors are implicitly local #4345
      && !escapedLocals(sym)
    )

    /** Validate variance of info of symbol `base` */
    private def validateVariance(base: Symbol) {
      // A flag for when we're in a refinement, meaning method parameter types
      // need to be checked.
      var inRefinement = false

      /** The variance of a symbol occurrence of `tvar` seen at the level of the definition of `base`.
       *  The search proceeds from `base` to the owner of `tvar`.
       *  Initially the state is covariant, but it might change along the search.
       *
       *  An alias which does not override anything is treated as Bivariant;
       *  this is OK because we always expand aliases for variance checking.
       *  However if it does override a type in a base class, we must assume Invariant
       *  because there may be references to the type parameter that are not checked.
       */
      def relativeVariance(tvar: Symbol): Variance = {
        def nextVariance(sym: Symbol, v: Variance): Variance = (
          if (shouldFlip(sym, tvar)) v.flip
          else if (isLocalOnly(sym)) Bivariant
          else if (!sym.isAliasType) v
          else if (sym.isOverridingSymbol) Invariant
          else Bivariant
        )
        def loop(sym: Symbol, v: Variance): Variance = (
          if (sym == tvar.owner || v.isBivariant) v
          else loop(sym.owner, nextVariance(sym, v))
        )
        loop(base, Covariant)
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
        case TypeRef(_, sym, _) if sym.isAliasType => validateVariance(tp.normalize, variance)

        case TypeRef(pre, sym, args) =>
          if (!sym.variance.isInvariant) {
            val relative = relativeVariance(sym)
            val required = relative * variance
            if (!relative.isBivariant) {
              log(s"verifying $sym (${sym.variance}${sym.locationString}) is $required at $base in ${base.owner}")
              if (sym.variance != required)
                issueVarianceError(base, sym, required)
            }
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
      def local = tree.symbol.hasLocalFlag
      tree match {
        case ClassDef(_, _, _, _) | TypeDef(_, _, _, _) =>
          validateVariance(tree.symbol)
          super.traverse(tree)
        // ModuleDefs need not be considered because they have been eliminated already
        case ValDef(_, _, _, _) =>
          if (!local)
            validateVariance(tree.symbol)
        case DefDef(_, _, tparams, vparamss, _, _) =>
          // No variance check for object-private/protected methods/values.
          if (!local) {
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
