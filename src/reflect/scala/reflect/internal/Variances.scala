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

package scala
package reflect
package internal

import Variance._
import scala.annotation.tailrec
import scala.reflect.internal.util.ReusableInstance

/** See comments at scala.reflect.internal.Variance.
 */
trait Variances {
  self: SymbolTable =>

  /** Used in Refchecks.
   *  TODO - eliminate duplication with varianceInType
   */
  class VarianceValidator extends InternalTraverser {
    // A flag for when we're in a refinement, meaning method parameter types
    // need to be checked.
    private[this] var inRefinement = false
    @inline private def withinRefinement(body: => Type): Type = {
      val saved = inRefinement
      inRefinement = true
      try body finally inRefinement = saved
    }

    protected def issueVarianceError(base: Symbol, sym: Symbol, required: Variance, tpe: Type): Unit = ()

    // Flip occurrences of type parameters and parameters, unless
    //  - it's a constructor, or case class factory or extractor
    //  - it's a type parameter / parameter of a local definition
    //  - it's a type parameter of tvar's owner.
    def shouldFlip(sym: Symbol, tvar: Symbol) = (
         sym.isParameter
      && !sym.owner.isLocalToThis
      && !(tvar.isTypeParameterOrSkolem && sym.isTypeParameterOrSkolem && tvar.owner == sym.owner)
    )

    // Is `sym` is local to a term or is private[this] or protected[this]?
    def isExemptFromVariance(sym: Symbol): Boolean =
      // super accessors are implicitly local #4345
      !sym.owner.isClass || sym.isLocalToThis || sym.isSuperAccessor

    private object ValidateVarianceMap extends VariancedTypeMap {
      private[this] var base: Symbol = _
      private[this] var inLowerBoundOf: Symbol = _

      /** The variance of a symbol occurrence of `tvar` seen at the level of the definition of `base`.
       *  The search proceeds from `base` to the owner of `tvar`.
       *  Initially the state is covariant, but it might change along the search.
       *
       *  A local alias type is treated as Bivariant;
       *  this is OK because such aliases are expanded for variance checking.
       *  However, for an alias which might be externally visible, we must assume Invariant,
       *  because there may be references to the type parameter that are not checked,
       *  leading to unsoundness (see scala/bug#6566).
       */
      def relativeVariance(tvar: Symbol): Variance = {
        def nextVariance(sym: Symbol, v: Variance): Variance =
          if (shouldFlip(sym, tvar)) v.flip
          else if (isExemptFromVariance(sym)) Bivariant
          else if (sym.isAliasType) Invariant
          else v

        @tailrec
        def loop(sym: Symbol, v: Variance): Variance =
          if (v.isBivariant) v
          else if (sym == tvar.owner)
            // We can't move this to `shouldFlip`, because it's needed only once at the end.
            if (inLowerBoundOf == sym) v.flip else v
          else loop(sym.owner, nextVariance(sym, v))

        loop(base, Covariant)
      }

      def isUncheckedVariance(tp: Type) = tp match {
        case AnnotatedType(annots, _)    => annots exists (_ matches definitions.uncheckedVarianceClass)
        case _                           => false
      }

      private def checkVarianceOfSymbol(sym: Symbol): Unit = {
        val relative = relativeVariance(sym)
        val required = relative * variance
        if (!relative.isBivariant) {
          def sym_s  = s"$sym (${sym.variance}${sym.locationString})"
          def base_s = s"$base in ${base.owner}" + (if (base.owner.isClass) "" else " in " + base.owner.enclClass)
          log(s"verifying $sym_s is $required at $base_s")
          if (sym.variance != required)
            issueVarianceError(base, sym, required, base.info)
        }
      }

      override def mapOver(decls: Scope): Scope = {
        decls foreach (sym => withVariance(if (sym.isAliasType) Invariant else variance)(this(sym.info)))
        decls
      }

      private def resultTypeOnly(tp: Type) = tp match {
        case _: MethodType => !inRefinement
        case _: PolyType   => true
        case _             => false
      }

      /** For PolyTypes, type parameters are skipped because they are defined
       *  explicitly (their TypeDefs will be passed here.) For MethodTypes, the
       *  same is true of the parameters (ValDefs) unless we are inside a
       *  refinement, in which case they are checked from here.
       */
      def apply(tp: Type): Type = {
        tp match {
          case _ if isUncheckedVariance(tp)                    =>
          case _ if resultTypeOnly(tp)                         => apply(tp.resultType)
          case TypeRef(_, sym, _) if shouldDealias(sym)        => apply(tp.normalize)
          case TypeRef(_, sym, _) if !sym.variance.isInvariant => checkVarianceOfSymbol(sym); tp.mapOver(this)
          case RefinedType(_, _)                               => withinRefinement(tp.mapOver(this))
          case ClassInfoType(parents, _, _)                    => parents.foreach(apply)
          case mt @ MethodType(_, result)                      => flipped(mt.paramTypes.foreach(apply)); apply(result)
          case _                                               => tp.mapOver(this)
        }
        // We're using TypeMap here for type traversal only. To avoid wasteful symbol
        // cloning during the recursion, it is important to return the input `tp`, rather
        // than the result of the pattern match above, which normalizes types.
        tp
      }

      private def shouldDealias(sym: Symbol): Boolean = {
        // The RHS of (private|protected)[this] type aliases are excluded from variance checks. This is
        // implemented in relativeVariance.
        // As such, we need to expand references to them to retain soundness. Example: neg/t8079a.scala
        sym.isAliasType && isExemptFromVariance(sym)
      }

      /** Validate the variance of types in the definition of `base`.
       *
       *  Traverse the type signature of `base` and for each type parameter:
       *    - Calculate the relative variance between `base` and the type parameter's owner by
       *      walking the owner chain of `base`.
       *    - Calculate the required variance of the type parameter which is the product of the
       *      relative variance and the current variance in the type signature of `base`.
       *    - Ensure that the declared variance of the type parameter is compatible with the
       *      required variance, otherwise issue an error.
       *
       *  Lower bounds need special handling. By default the variance is flipped when entering a
       *  lower bound. In most cases this is the correct behaviour except for the type parameters
       *  of higher-kinded types. E.g. in `Foo` below `x` occurs in covariant position:
       *    `class Foo[F[+_]] { type G[+x] >: F[x] }`
       *
       *  To handle this special case, track when entering the lower bound of a HKT in a variable
       *  and flip the relative variance for its type parameters. (flipping the variance a second
       *  time negates the first flip).
       */
      def validateDefinition(base: Symbol): Unit = {
        this.base = base
        base.info match {
          case PolyType(_, TypeBounds(lo, hi)) =>
            inLowerBoundOf = base
            try flipped(apply(lo))
            finally inLowerBoundOf = null
            apply(hi)
          case other =>
            apply(other)
        }
      }
    }

    private object PolyTypeVarianceMap extends TypeMap {

      private def ownerOf(pt: PolyType): Symbol =
        pt.typeParams.head.owner

      private def checkPolyTypeParam(pt: PolyType, tparam: Symbol, tpe: Type): Unit =
        if (!tparam.isInvariant) {
          val required = varianceInType(tpe, considerUnchecked = true)(tparam)
          if (!required.isBivariant && tparam.variance != required)
            issueVarianceError(ownerOf(pt), tparam, required, pt)
        }

      def apply(tp: Type): Type = {
        tp match {
          case pt @ PolyType(typeParams, TypeBounds(lo, hi)) =>
            typeParams.foreach { tparam =>
              checkPolyTypeParam(pt, tparam, lo)
              checkPolyTypeParam(pt, tparam, hi)
            }

            pt.mapOver(this)

          case pt @ PolyType(typeParams, resultType) =>
            typeParams.foreach(checkPolyTypeParam(pt, _, resultType))
            pt.mapOver(this)

          case _ =>
            tp.mapOver(this)
        }

        tp
      }
    }

    /** Validate the variance of (the type parameters of) PolyTypes in `tpe`.
     *
     *  `validateDefinition` cannot handle PolyTypes in arbitrary position, because in general
     *  the relative variance of such types cannot be computed by walking the owner chain.
     *
     *  Instead this method applies a naive algorithm which is correct but less efficient:
     *  use `varianceInType` to check each type parameter of a PolyType separately.
     */
    def validateVarianceOfPolyTypesIn(tpe: Type): Unit =
      PolyTypeVarianceMap(tpe)

    override def traverse(tree: Tree): Unit = {
      def sym = tree.symbol
      // No variance check for object-private/protected methods/values.
      // Or constructors, or case class factory or extractor.
      def skip = (
           sym == NoSymbol
        || sym.owner.isConstructor                  // FIXME: this is unsound - scala/bug#8737
        || sym.owner.isCaseApplyOrUnapply           // same treatment as constructors
        || sym.isParamAccessor && sym.isLocalToThis // local class parameters are construction only
      )

      tree match {
        case _: MemberDef if skip =>
          debuglog(s"Skipping variance check of ${sym.defString}")
        case ClassDef(_, _, _, _) | TypeDef(_, _, _, _) =>
          ValidateVarianceMap.validateDefinition(sym)
          tree.traverse(this)
        case ModuleDef(_, _, _) =>
          ValidateVarianceMap.validateDefinition(sym.moduleClass)
          tree.traverse(this)
        case ValDef(_, _, _, _) =>
          ValidateVarianceMap.validateDefinition(sym)
        case DefDef(_, _, tparams, vparamss, _, _) =>
          ValidateVarianceMap.validateDefinition(sym)
          traverseTrees(tparams)
          traverseTreess(vparamss)
        case Template(_, _, _) =>
          tree.traverse(this)
        case CompoundTypeTree(_) =>
          tree.traverse(this)
        // scala/bug#7872 These two cases make sure we don't miss variance exploits
        // in originals, e.g. in `foo[({type l[+a] = List[a]})#l]`
        case tt @ TypeTree() if tt.original != null =>
          tt.original.traverse(this)
        case tt: TypTree =>
          tt.traverse(this)
        case _ =>
      }
    }
  }

  /** Compute variance of type parameter `tparam` in all types `tps`. */
  final def varianceInTypes(tps: List[Type])(tparam: Symbol): Variance =
    Variance.foldExtract(tps)(t => varianceInType(t)(tparam))

  /** Compute variance of type parameter `tparam` in type `tp`. */
  final def varianceInType(tp: Type, considerUnchecked: Boolean = false)(tparam: Symbol): Variance =
    varianceInTypeCache.using(_.apply(tp, tparam, considerUnchecked))

  private[this] val varianceInTypeCache = ReusableInstance[varianceInType](new varianceInType, enabled = isCompilerUniverse)

  private final class varianceInType {
    private[this] var tp: Type = _
    private[this] var tparam: Symbol = _
    private[this] var considerUnchecked = false

    import Variance._
    private def inArgs(sym: Symbol, args: List[Type]): Variance = foldExtract2(args, sym.typeParams)(inArgParam)
    private def inSyms(syms: List[Symbol]): Variance            = foldExtract(syms)(inSym)
    private def inTypes(tps: List[Type]): Variance              = foldExtract(tps)(inType)
    private def inAnnots(anns: List[AnnotationInfo]): Variance  = foldExtract(anns)(inAnnotationAtp)
    private def unchecked(anns: List[AnnotationInfo]): Boolean  = considerUnchecked && anns.exists(_.matches(definitions.uncheckedVarianceClass))

    // OPT these extractors are hoisted to fields to reduce allocation. We're also avoiding Function1[_, Variance] to
    //     avoid value class boxing.
    private[this] lazy val inAnnotationAtp: Extractor[AnnotationInfo] = (a: AnnotationInfo) => inType(a.atp)
    private[this] lazy val inArgParam: Extractor2[Type, Symbol]       = (a, b) => inType(a) * b.variance
    private[this] lazy val inSym: Extractor[Symbol]                   = (sym: Symbol) => if (sym.isAliasType) inType(sym.info).cut else inType(sym.info)
    private[this] val inType: Extractor[Type] = {
      case pt: ProtoType                                   => inType(pt.toVariantType)
      case ErrorType | NoType | NoPrefix                   => Bivariant
      case ThisType(_) | ConstantType(_)                   => Bivariant
      case TypeRef(_, tparam, _) if tparam eq this.tparam  => Covariant
      case NullaryMethodType(restpe)                       => inType(restpe)
      case SingleType(pre, _)                              => inType(pre)
      case TypeRef(pre, _, _) if tp.isHigherKinded         => inType(pre)          // a type constructor cannot occur in tp's args
      case TypeRef(pre, sym, args)                         => inType(pre)          & inArgs(sym, args)
      case TypeBounds(lo, hi)                              => inType(lo).flip      & inType(hi)
      case RefinedType(parents, defs)                      => inTypes(parents)     & inSyms(defs.toList)
      case MethodType(params, restpe)                      => inSyms(params).flip  & inType(restpe)
      case PolyType(tparams, restpe)                       => inSyms(tparams).flip & inType(restpe)
      case ExistentialType(tparams, restpe)                => inSyms(tparams)      & inType(restpe)
      case AnnotatedType(annots, _) if unchecked(annots)   => Bivariant
      case AnnotatedType(annots, tp)                       => inAnnots(annots)     & inType(tp)
      case SuperType(thistpe, supertpe)                    => inType(thistpe)      & inType(supertpe)
      case x                                               => throw new MatchError(x)
    }

    def apply(tp: Type, tparam: Symbol, considerUnchecked: Boolean): Variance = {
      this.tp = tp
      this.tparam = tparam
      this.considerUnchecked = considerUnchecked
      try inType(tp)
      finally {
        this.tp = null
        this.tparam = null
        this.considerUnchecked = false
      }
    }
  }
}
