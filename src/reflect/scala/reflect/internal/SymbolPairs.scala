/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala
package reflect
package internal

import scala.collection.mutable
import util.HashSet
import scala.annotation.tailrec

/** An abstraction for considering symbol pairs.
 *  One of the greatest sources of compiler bugs is that symbols can
 *  trivially lose their prefixes and turn into some completely different
 *  type with the smallest of errors. It is the exception not the rule
 *  that type comparisons are done correctly.
 *
 *  This offers a small step toward coherence with two abstractions
 *  which come up over and over again:
 *
 *    RelativeTo: operations relative to a prefix
 *    SymbolPair: two symbols being related somehow, plus the class
 *       in which the relation is being performed
 *
 *  This is only a start, but it is a start.
 */
abstract class SymbolPairs {
  val global: SymbolTable
  import global._

  /** Type operations relative to a prefix.  All operations work on Symbols,
   *  and the types are the member types of those symbols in the prefix.
   */
  class RelativeTo(val prefix: Type) {
    def this(clazz: Symbol) = this(clazz.thisType)
    import scala.language.implicitConversions // geez, it even has to hassle me when it's private
    private implicit def symbolToType(sym: Symbol): Type = prefix memberType sym

    def erasureOf(sym: Symbol): Type         = erasure.erasure(sym)(sym: Type)
    def signature(sym: Symbol): String       = sym defStringSeenAs (sym: Type)
    def erasedSignature(sym: Symbol): String = sym defStringSeenAs erasureOf(sym)

    def isSameType(sym1: Symbol, sym2: Symbol): Boolean    = sym1 =:= sym2
    def isSubType(sym1: Symbol, sym2: Symbol): Boolean     = sym1 <:< sym2
    def isSuperType(sym1: Symbol, sym2: Symbol): Boolean   = sym2 <:< sym1
    def isSameErasure(sym1: Symbol, sym2: Symbol): Boolean = erasureOf(sym1) =:= erasureOf(sym2)
    def matches(sym1: Symbol, sym2: Symbol): Boolean       = (sym1: Type) matches (sym2: Type)

    override def toString = s"RelativeTo($prefix)"
  }

  /** Are types tp1 and tp2 equivalent seen from the perspective
   *  of `baseClass`? For instance List[Int] and Seq[Int] are =:=
   *  when viewed from IterableClass.
   */
  def sameInBaseClass(baseClass: Symbol)(tp1: Type, tp2: Type) =
    (tp1 baseType baseClass) =:= (tp2 baseType baseClass)

  case class SymbolPair(base: Symbol, low: Symbol, high: Symbol) {
    def pos                 = if (low.owner == base) low.pos else if (high.owner == base) high.pos else base.pos
    def self: Type          = base.thisType
    def rootType: Type      = base.thisType

    def lowType: Type       = self memberType low
    def lowErased: Type     = erasure.specialErasure(base)(low.tpe)
    def lowClassBound: Type = classBoundAsSeen(low.tpe.typeSymbol)

    def highType: Type       = self memberType high
    def highInfo: Type       = self memberInfo high
    def highErased: Type     = erasure.specialErasure(base)(high.tpe)
    def highClassBound: Type = classBoundAsSeen(high.tpe.typeSymbol)

    def isErroneous = low.tpe.isErroneous || high.tpe.isErroneous
    def sameKind    = sameLength(low.typeParams, high.typeParams)

    private def classBoundAsSeen(tsym: Symbol) =
      tsym.classBound.asSeenFrom(rootType, tsym.owner)

    private def memberDefString(sym: Symbol, where: Boolean) = {
      val def_s = (
        if (sym.isConstructor) s"$sym: ${self memberType sym}"
        else sym defStringSeenAs (self memberType sym)
      )
      def_s + whereString(sym)
    }
    /** A string like ' at line 55' if the symbol is defined in the class
     *  under consideration, or ' in trait Foo' if defined elsewhere.
     */
    private def whereString(sym: Symbol) =
      if (sym.owner == base) " at line " + sym.pos.line else sym.locationString

    def lowString  = memberDefString(low, where = true)
    def highString = memberDefString(high, where = true)

    override def toString = sm"""
      |Cursor(in $base) {
      |   high  $highString
      | erased  $highErased
      |  infos  ${high.infosString}
      |    low  $lowString
      | erased  $lowErased
      |  infos  ${low.infosString}
      |}""".trim
  }

  /** The cursor class
   *  @param base   the base class containing the participating symbols
   */
  abstract class Cursor(val base: Symbol) {
    cursor =>

      final val self  = base.thisType   // The type relative to which symbols are seen.
    private val decls = newScope        // all the symbols which can take part in a pair.
    private val size  = bases.length

    /** A symbol for which exclude returns true will not appear as
     *  either end of a pair.
     */
    protected def exclude(sym: Symbol): Boolean

    /** Does `sym1` match `sym2` such that (sym1, sym2) should be
     *  considered as a (lo, high) pair? Types always match. Term symbols
     *  match if their member types relative to `self` match.
     */
    protected def matches(lo: Symbol, high: Symbol): Boolean

    /** The parents and base classes of `base`.  Can be refined in subclasses.
     */
    protected def parents: List[Type] = base.info.parents
    protected def bases: List[Symbol] = base.info.baseClasses

    /** An implementation of BitSets as arrays (maybe consider collection.BitSet
     *  for that?) The main purpose of this is to implement
     *  intersectionContainsElement efficiently.
     */
    private type BitSet = Array[Int]

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

    /** A map from baseclasses of <base> to ints, with smaller ints meaning lower in
     *  linearization order. Symbols that are not baseclasses map to -1.
     */
    private val index = new mutable.HashMap[Symbol, Int] { override def default(key: Symbol) = -1 }

    /** The scope entries that have already been visited as highSymbol
     *  (but may have been excluded via hasCommonParentAsSubclass.)
     *  These will not appear as lowSymbol.
     */
    private val visited = HashSet[ScopeEntry]("visited", 64)

    /** Initialization has to run now so decls is populated before
     *  the declaration of curEntry.
     */
    init()

    // The current low and high symbols; the high may be null.
    private[this] var lowSymbol: Symbol  = _
    private[this] var highSymbol: Symbol = _

    // The current entry candidates for low and high symbol.
    private[this] var curEntry  = decls.elems
    private[this] var nextEntry = curEntry

    // These fields are initially populated with a call to next().
    next()

    // populate the above data structures
    private def init() {
      // Fill `decls` with lower symbols shadowing higher ones
      def fillDecls(bcs: List[Symbol], deferred: Boolean) {
        if (!bcs.isEmpty) {
          fillDecls(bcs.tail, deferred)
          var e = bcs.head.info.decls.elems
          while (e ne null) {
            if (e.sym.initialize.isDeferred == deferred && !exclude(e.sym))
              decls enter e.sym
            e = e.next
          }
        }
      }
      var i = 0
      for (bc <- bases) {
        index(bc) = i
        subParents(i) = new BitSet(size)
        i += 1
      }
      for (p <- parents) {
        val pIndex = index(p.typeSymbol)
        if (pIndex >= 0)
          for (bc <- p.baseClasses ; if sameInBaseClass(bc)(p, self)) {
            val bcIndex = index(bc)
            if (bcIndex >= 0)
              include(subParents(bcIndex), pIndex)
          }
      }
      // first, deferred (this will need to change if we change lookup rules!)
      fillDecls(bases, deferred = true)
      // then, concrete.
      fillDecls(bases, deferred = false)
    }

    private def include(bs: BitSet, n: Int) {
      val nshifted = n >> 5
      val nmask    = 1 << (n & 31)
      bs(nshifted) |= nmask
    }

    /** Implements `bs1 * bs2 * {0..n} != 0`.
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

    /** Do `sym1` and `sym2` have a common subclass in `parents`?
     *  In that case we do not follow their pairs.
     */
    private def hasCommonParentAsSubclass(sym1: Symbol, sym2: Symbol) = {
      val index1 = index(sym1.owner)
      (index1 >= 0) && {
        val index2 = index(sym2.owner)
        (index2 >= 0) && {
          intersectionContainsElementLeq(
            subParents(index1), subParents(index2), index1 min index2)
        }
      }
    }

    @tailrec private def advanceNextEntry() {
      if (nextEntry ne null) {
        nextEntry = decls lookupNextEntry nextEntry
        if (nextEntry ne null) {
          val high    = nextEntry.sym
          val isMatch = matches(lowSymbol, high) && { visited addEntry nextEntry ; true } // side-effect visited on all matches

          // skip nextEntry if a class in `parents` is a subclass of the
          // owners of both low and high.
          if (isMatch && !hasCommonParentAsSubclass(lowSymbol, high))
            highSymbol = high
          else
            advanceNextEntry()
        }
      }
    }
    @tailrec private def advanceCurEntry() {
      if (curEntry ne null) {
        curEntry = curEntry.next
        if (curEntry ne null) {
          if (visited(curEntry) || exclude(curEntry.sym))
            advanceCurEntry()
          else
            nextEntry = curEntry
        }
      }
    }

    /** The `low` and `high` symbol.  In the context of overriding pairs,
     *  low == overriding and high == overridden.
     */
    def low  = lowSymbol
    def high = highSymbol

    def hasNext     = curEntry ne null
    def currentPair = new SymbolPair(base, low, high)
    def iterator    = new Iterator[SymbolPair] {
      def hasNext = cursor.hasNext
      def next()  = try cursor.currentPair finally cursor.next()
    }

    // Note that next is called once during object initialization to
    // populate the fields tracking the current symbol pair.
    def next() {
      if (curEntry ne null) {
        lowSymbol = curEntry.sym
        advanceNextEntry()        // sets highSymbol
        if (nextEntry eq null) {
          advanceCurEntry()
          next()
        }
      }
    }
  }
}
