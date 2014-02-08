/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal

import scala.collection.{ mutable, immutable }

/** The name of this trait defines the eventual intent better than
 *  it does the initial contents.
 */
trait ExistentialsAndSkolems {
  self: SymbolTable =>

  /** Map a list of type parameter symbols to skolemized symbols, which
   *  can be deskolemized to the original type parameter. (A skolem is a
   *  representation of a bound variable when viewed inside its scope.)
   *  !!!Adriaan: this does not work for hk types.
   *
   *  Skolems will be created at level 0, rather than the current value
   *  of `skolemizationLevel`. (See SI-7782)
   */
  def deriveFreshSkolems(tparams: List[Symbol]): List[Symbol] = {
    class Deskolemizer extends LazyType {
      override val typeParams = tparams
      val typeSkolems  = typeParams map (_.newTypeSkolem setInfo this)
      override def complete(sym: Symbol) {
        // The info of a skolem is the skolemized info of the
        // actual type parameter of the skolem
        sym setInfo sym.deSkolemize.info.substSym(typeParams, typeSkolems)
      }
    }

    val saved = skolemizationLevel
    skolemizationLevel = 0
    try new Deskolemizer().typeSkolems
    finally skolemizationLevel = saved
  }

  def isRawParameter(sym: Symbol) = // is it a type parameter leaked by a raw type?
    sym.isTypeParameter && sym.owner.isJavaDefined

  /** Given a set `rawSyms` of term- and type-symbols, and a type
   *  `tp`, produce a set of fresh type parameters and a type so that
   *  it can be abstracted to an existential type. Every type symbol
   *  `T` in `rawSyms` is mapped to a clone. Every term symbol `x` of
   *  type `T` in `rawSyms` is given an associated type symbol of the
   *  following form:
   *
   *    type x.type <: T with Singleton
   *
   *  The name of the type parameter is `x.type`, to produce nice
   *  diagnostics. The Singleton parent ensures that the type
   *  parameter is still seen as a stable type. Type symbols in
   *  rawSyms are fully replaced by the new symbols. Term symbols are
   *  also replaced, except for term symbols of an Ident tree, where
   *  only the type of the Ident is changed.
   */
  final def existentialTransform[T](rawSyms: List[Symbol], tp: Type, rawOwner: Symbol = NoSymbol)(creator: (List[Symbol], Type) => T): T = {
    /** If we map a set of hidden symbols to their existential bounds, we
     *  have a problem: the bounds may themselves contain references to the
     *  hidden symbols.  So this recursively calls existentialBound until
     *  the typeSymbol is not amongst the symbols being hidden.
     */
    object deepBound extends TypeMap {
      def safeExistentialBound(tp: Type): Type = {
        val sym = tp.typeSymbol
        if (rawSyms contains sym) safeExistentialBound(sym.existentialBound.bounds.hi)
        else tp
      }

      def apply(tp: Type) = mapOver(safeExistentialBound(tp))
    }

    val quantifiers = rawSyms map { sym =>
      val name = sym.name match {
        case x: TypeName  => x
        case x            => tpnme.singletonName(x)
      }
      def rawOwner0 = rawOwner orElse abort(s"no owner provided for existential transform over raw parameter: $sym")
      val sowner    = if (isRawParameter(sym)) rawOwner0 else sym.owner
      val quant     = sowner.newExistential(name, sym.pos)
      val bound     = sym.existentialBound match {
        // Hanging onto lower bound in case anything interesting happens with it.
        case TypeBounds(lo, hi) => TypeBounds(lo, deepBound(hi))
        case tp                 => deepBound(tp.bounds.hi)
      }

      quant setInfo bound.cloneInfo(quant)
    }

    // Higher-kinded existentials are not yet supported, but this is
    // tpeHK for when they are: "if a type constructor is expected/allowed,
    // tpeHK must be called instead of tpe."
    val quantifierTypes = quantifiers map (_.tpeHK)

    // TODO: fuse the substitutions? both are needed!
    // the first one replaces types, the second one is motivated by the example below
    def doSubst(info: Type) = info.subst(rawSyms, quantifierTypes).substSym(rawSyms, quantifiers)

    /* Abandon all hope for symbol consistency, ye who enter here:
     * For example, from SI-6493, let's compute a result type for
     *   `def foo = { class Foo { class Bar { val b = 2 }}; val f = new Foo; new f.Bar }`:
     *
     * Note the inconsistent symbol ids in:
     *
     * rawSyms     = List(value f#49740, class Foo#49739, class Bar#49742)
     * quantifiers + their infos =
     *       type f.type#49774 <: AnyRef{type Bar#49762 <: AnyRef{val b#49764: Int}} with Singleton,
     *       type Foo#49775    <: AnyRef{type Bar#49770 <: AnyRef{val b#49772: Int}},
     *                                   type Bar#49773 <: AnyRef{val b#49758: Int}
     *
     *
     * It makes sense to subst the symbols in tp, but the modifyInfo will have little effect on quantifiers.
     * Luckily, coevolveSym will fix up some of the inconsistencies, so that we get
     * `f.type#49774.Bar#49762`, which is extrapolated to `AnyRef{type Bar#49868 <: AnyRef{val b: Int}}#Bar#49868`.
     *
     * The intermediate type `f.type#49774.Bar#49762` motivates the need for the `.substSym` above,
     * as `.subst` does not touch the `Bar#49742` in the `f#49740.Bar#49742` that we start out with.
     *
     * The remaining problem is that ExistentialExtrapolation will not consider type Bar#49868 as existentially bound.
     * Instead, it's looking for the stale version in quantifiers, Bar#49773.
     * If it was looking for Bar#49868, the result type would simply be `AnyRef{val b: Int}`
     *
     * TODO: figure out how to compute the full set of quantifiers for extrapolation
     *       the `doSubst(tp)` will clone more symbols, so we'll probably need to traverse its result and
     *       somehow correlate the contained symbols to those in `quantifiers`...
     */
    creator(quantifiers map (_ modifyInfo doSubst), doSubst(tp))
  }

  /**
   * Compute an existential type from hidden symbols `hidden` and type `tp`.
   * @param hidden   The symbols that will be existentially abstracted
   * @param hidden   The original type
   * @param rawOwner The owner for Java raw types.
   */
  final def packSymbols(hidden: List[Symbol], tp: Type, rawOwner: Symbol = NoSymbol): Type =
    if (hidden.isEmpty) tp
    else existentialTransform(hidden, tp, rawOwner)(existentialAbstraction)
}
