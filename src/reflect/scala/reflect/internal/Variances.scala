/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
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
    // A flag for when we're in a refinement, meaning method parameter types
    // need to be checked.
    private var inRefinement = false
    @inline private def withinRefinement(body: => Type): Type = {
      val saved = inRefinement
      inRefinement = true
      try body finally inRefinement = saved
    }

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
      && !(tvar.isTypeParameterOrSkolem && sym.isTypeParameterOrSkolem && tvar.owner == sym.owner)
    )
    // return Bivariant if `sym` is local to a term
    // or is private[this] or protected[this]
    def isLocalOnly(sym: Symbol) = !sym.owner.isClass || (
         sym.isTerm
      && (sym.hasLocalFlag || sym.isSuperAccessor) // super accessors are implicitly local #4345
      && !escapedLocals(sym)
    )

    private object ValidateVarianceMap extends TypeMap(trackVariance = true) {
      private var base: Symbol = _

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
          else if (sym.isAliasType) Invariant
          else v
        )
        def loop(sym: Symbol, v: Variance): Variance = (
          if (sym == tvar.owner || v.isBivariant) v
          else loop(sym.owner, nextVariance(sym, v))
        )
        loop(base, Covariant)
      }
      def isUncheckedVariance(tp: Type) = tp match {
        case AnnotatedType(annots, _, _) => annots exists (_ matches definitions.uncheckedVarianceClass)
        case _                           => false
      }

      private def checkVarianceOfSymbol(sym: Symbol) {
        val relative = relativeVariance(sym)
        val required = relative * variance
        if (!relative.isBivariant) {
          def sym_s  = s"$sym (${sym.variance}${sym.locationString})"
          def base_s = s"$base in ${base.owner}" + (if (base.owner.isClass) "" else " in " + base.owner.enclClass)
          log(s"verifying $sym_s is $required at $base_s")
          if (sym.variance != required)
            issueVarianceError(base, sym, required)
        }
      }
      override def mapOver(decls: Scope): Scope = {
        decls foreach (sym => withVariance(if (sym.isAliasType) Invariant else variance)(this(sym.info)))
        decls
      }
      private def resultTypeOnly(tp: Type) = tp match {
        case mt: MethodType => !inRefinement
        case pt: PolyType   => true
        case _              => false
      }

      /** For PolyTypes, type parameters are skipped because they are defined
       *  explicitly (their TypeDefs will be passed here.) For MethodTypes, the
       *  same is true of the parameters (ValDefs) unless we are inside a
       *  refinement, in which case they are checked from here.
       */
      def apply(tp: Type): Type = tp match {
        case _ if isUncheckedVariance(tp)                    => tp
        case _ if resultTypeOnly(tp)                         => this(tp.resultType)
        case TypeRef(_, sym, _) if sym.isAliasType           => this(tp.normalize)
        case TypeRef(_, sym, _) if !sym.variance.isInvariant => checkVarianceOfSymbol(sym) ; mapOver(tp)
        case RefinedType(_, _)                               => withinRefinement(mapOver(tp))
        case ClassInfoType(parents, _, _)                    => parents foreach this ; tp
        case mt @ MethodType(_, result)                      => flipped(mt.paramTypes foreach this) ; this(result)
        case _                                               => mapOver(tp)
      }
      def validateDefinition(base: Symbol) {
        val saved = this.base
        this.base = base
        try apply(base.info)
        finally this.base = saved
      }
    }

    /** Validate variance of info of symbol `base` */
    private def validateVariance(base: Symbol) {
      ValidateVarianceMap validateDefinition base
    }

    override def traverse(tree: Tree) {
      def sym = tree.symbol
      // No variance check for object-private/protected methods/values.
      // Or constructors, or case class factory or extractor.
      def skip = (
           sym.hasLocalFlag
        || sym.owner.isConstructor
        || sym.owner.isCaseApplyOrUnapply
      )
      tree match {
        case defn: MemberDef if skip =>
          debuglog(s"Skipping variance check of ${sym.defString}")
        case ClassDef(_, _, _, _) | TypeDef(_, _, _, _) =>
          validateVariance(sym)
          super.traverse(tree)
        // ModuleDefs need not be considered because they have been eliminated already
        case ValDef(_, _, _, _) =>
          validateVariance(sym)
        case DefDef(_, _, tparams, vparamss, _, _) =>
          validateVariance(sym)
          traverseTrees(tparams)
          traverseTreess(vparamss)
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
