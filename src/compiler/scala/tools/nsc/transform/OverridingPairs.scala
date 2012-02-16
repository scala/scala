/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.collection.mutable
import symtab.Flags._
import util.HashSet
import annotation.tailrec

/** A class that yields a kind of iterator (`Cursor`),
 *  which yields all pairs of overriding/overridden symbols
 *  that are visible in some baseclass, unless there's a parent class
 *  that already contains the same pairs.
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class OverridingPairs {

  val global: Global
  import global._

  /** The cursor class
   *  @param base   the base class that contains the overriding pairs
   */
  class Cursor(base: Symbol) {

    private val self = base.thisType

    /** Symbols to exclude: Here these are constructors, private locals,
     *  and bridges. But it may be refined in subclasses.
     *
     */
    protected def exclude(sym: Symbol): Boolean =
      sym.isConstructor || sym.isPrivateLocal || sym.hasFlag(BRIDGE)

    /** The parents of base (may also be refined).
     */
    protected def parents: List[Type] = base.info.parents

    /** Does `sym1` match `sym2` so that it qualifies as overriding.
     *  Types always match. Term symbols match if their membertypes
     *  relative to <base>.this do
     */
    protected def matches(sym1: Symbol, sym2: Symbol): Boolean = {
      def tp_s(s: Symbol) = self.memberType(s) + "/" + self.memberType(s).getClass
      val result = sym1.isType || (self.memberType(sym1) matches self.memberType(sym2))
      debuglog("overriding-pairs? %s matches %s (%s vs. %s) == %s".format(
        sym1.fullLocationString, sym2.fullLocationString, tp_s(sym1), tp_s(sym2), result))
      
      result
    }

    /** An implementation of BitSets as arrays (maybe consider collection.BitSet
     *  for that?) The main purpose of this is to implement
     *  intersectionContainsElement efficiently.
     */
    private type BitSet = Array[Int]

    private def include(bs: BitSet, n: Int) {
      val nshifted = n >> 5
      val nmask = 1 << (n & 31)
      bs(nshifted) = bs(nshifted) | nmask
    }

    /** Implements `bs1 * bs2 * {0..n} != 0.
     *  Used in hasCommonParentAsSubclass */
    private def intersectionContainsElementLeq(bs1: BitSet, bs2: BitSet, n: Int): Boolean = {
      val nshifted = n >> 5
      val nmask = 1 << (n & 31)
      var i = 0
      while (i < nshifted) {
        if ((bs1(i) & bs2(i)) != 0) return true
        i += 1
      }
      (bs1(nshifted) & bs2(nshifted) & (nmask | nmask - 1)) != 0
    }

    /** The symbols that can take part in an overriding pair */
    private val decls = newScope

    // fill `decls` with overriding shadowing overridden */
    { def fillDecls(bcs: List[Symbol], deferredflag: Int) {
        if (!bcs.isEmpty) {
          fillDecls(bcs.tail, deferredflag)
          var e = bcs.head.info.decls.elems;
          while (e ne null) {
            if (e.sym.getFlag(DEFERRED) == deferredflag.toLong && !exclude(e.sym))
              decls enter e.sym;
            e = e.next
          }
        }
      }
      // first, deferred (this wil need to change if we change lookup rules!
      fillDecls(base.info.baseClasses, DEFERRED)
      // then, concrete.
      fillDecls(base.info.baseClasses, 0)
    }

    private val size = base.info.baseClasses.length

    /** A map from baseclasses of <base> to ints, with smaller ints meaning lower in
     *  linearization order.
     */
    private val index = new mutable.HashMap[Symbol, Int]

    // Note: overridingPairs can be called at odd instances by the Eclipse plugin
    // Soemtimes symbols are not yet defined and we get missing keys.
    // The implementation here is hardened so that it does not crash on a missing key.

    { var i = 0
      for (bc <- base.info.baseClasses) {
        index(bc) = i
        i += 1
      }
    }

    /** A mapping from all base class indices to a bitset
     *  which indicates whether parents are subclasses.
     *
     *   i \in subParents(j)   iff
     *   exists p \in parents, b \in baseClasses:
     *     i = index(p)
     *     j = index(b)
     *     p isSubClass b
     *     p.baseType(b) == self.baseType(b)
     */
    private val subParents = new Array[BitSet](size)

    { for (i <- List.range(0, size))
        subParents(i) = new BitSet(size);
      for (p <- parents) {
        index get p.typeSymbol match {
          case Some(pIndex) =>
            for (bc <- p.baseClasses)
              if (p.baseType(bc) =:= self.baseType(bc))
                index get bc match {
                  case Some(bcIndex) =>
                    include(subParents(bcIndex), pIndex)
                  case None =>
                }
              else debuglog("SKIPPING "+p+" -> "+p.baseType(bc)+" / "+self.baseType(bc)+" from "+base)
          case None =>
        }
      }
   }

    /** Do `sym1` and `sym2` have a common subclass in `parents`?
     *  In that case we do not follow their overriding pairs
     */
    private def hasCommonParentAsSubclass(sym1: Symbol, sym2: Symbol) = (
      for (index1 <- index get sym1.owner ; index2 <- index get sym2.owner) yield
        intersectionContainsElementLeq(subParents(index1), subParents(index2), index1 min index2)
    ).exists(_ == true)

    /** The scope entries that have already been visited as overridden
     *  (maybe excluded because of hasCommonParentAsSubclass).
     *  These will not appear as overriding
     */
    private val visited = HashSet[ScopeEntry]("visited", 64)

    /** The current entry candidate for overriding
     */
    private var curEntry = decls.elems

    /** The current entry candidate for overridden */
    private var nextEntry = curEntry

    /** The current candidate symbol for overriding */
    var overriding: Symbol = _

    /** If not null: The symbol overridden by overriding */
    var overridden: Symbol = _

    //@M: note that next is called once during object initialization
    def hasNext: Boolean = curEntry ne null

    @tailrec
    final def next() {
      if (curEntry ne null) {
        overriding = curEntry.sym
        if (nextEntry ne null) {
          do {
            do {
              nextEntry = decls.lookupNextEntry(nextEntry);
              /* DEBUG
              if ((nextEntry ne null) &&
                  !(nextEntry.sym hasFlag PRIVATE) &&
                  !(overriding.owner == nextEntry.sym.owner) &&
                  !matches(overriding, nextEntry.sym))
                println("skipping "+overriding+":"+self.memberType(overriding)+overriding.locationString+" to "+nextEntry.sym+":"+self.memberType(nextEntry.sym)+nextEntry.sym.locationString)
              */
              } while ((nextEntry ne null) &&
                     ((nextEntry.sym hasFlag PRIVATE) ||
                      (overriding.owner == nextEntry.sym.owner) ||
                      (!matches(overriding, nextEntry.sym)) ||
                      (exclude(overriding))))
            if (nextEntry ne null) visited addEntry nextEntry
            // skip nextEntry if a class in `parents` is a subclass of the owners of both
            // overriding and nextEntry.sym
          } while ((nextEntry ne null) && (hasCommonParentAsSubclass(overriding, nextEntry.sym)))
          if (nextEntry ne null) {
            overridden = nextEntry.sym;
            //Console.println("yield: " + overriding + overriding.locationString + " / " + overridden + overridden.locationString);//DEBUG
          } else {
            do {
              curEntry = curEntry.next
            } while ((curEntry ne null) && (visited contains curEntry));
            nextEntry = curEntry
            next
          }
        }
      }
    }

    next
  }
}
