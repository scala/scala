/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.reflect.internal.Variance, scala.reflect.internal.Variance._
import scala.collection.{ mutable, immutable }

/** See comments at scala.reflect.internal.Variance.
 */
trait Variances {
  val global: Global
  import global._
  import definitions.uncheckedVarianceClass

  class VarianceValidator extends Traverser {
    val escapedPrivateLocals = mutable.HashSet[Symbol]()

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

            if (!v.isBivariant && sym.variance != v * variance) {
              //Console.println("relativeVariance(" + base + "," + sym + ") = " + v);//DEBUG
              def tpString(tp: Type) = tp match {
                case ClassInfoType(parents, _, clazz) => "supertype "+intersectionType(parents, clazz.owner)
                case _ => "type "+tp
              }
              currentRun.currentUnit.error(base.pos,
                         sym.variance + " " + sym +
                         " occurs in " + (v * variance) +
                         " position in " + tpString(base.info) + " of " + base);
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
          if (!annots.exists(_ matches uncheckedVarianceClass))
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
