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

import util.{HashSet, StringContextStripMarginOps}
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

  final case class SymbolPair(base: Symbol, low: Symbol, high: Symbol) {
    private[this] val self  = base.thisType

    def pos                 = if (low.owner == base) low.pos else if (high.owner == base) high.pos else base.pos
    def rootType: Type      = self

    def lowType: Type       = self memberType low
    def lowErased: Type     = erasure.specialErasure(low)(low.tpe)
    def lowClassBound: Type = classBoundAsSeen(low.tpe.typeSymbol)

    def highType: Type       = self memberType high
    def highInfo: Type       = self memberInfo high
    def highErased: Type     = erasure.specialErasure(high)(high.tpe)
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
    private[this] val decls = newScope        // all the symbols which can take part in a pair.
    @annotation.unused
    private[this] val size  = bases.length

    /** A symbol for which exclude returns true will not appear as
     *  either end of a pair.
     */
    protected def exclude(sym: Symbol): Boolean

    /** Does `this.low` match `high` such that (low, high) should be
     *  considered as a pair? Types always match. Term symbols
     *  match if their member types relative to `self` match.
     */
    protected def matches(high: Symbol): Boolean

    /** Even if a pair `matches`, should the cursor skip this pair?
      *
      * @param lowClass owner of the next low symbol
      * @param highClass owner of the next hi symbol
      * @return whether to skip this pair
      */
    protected def skipOwnerPair(lowClass: Symbol, highClass: Symbol): Boolean = false

    protected def bases: List[Symbol] = base.info.baseClasses

    /** The scope entries that have already been visited as highSymbol
     *  (but may have been excluded via hasCommonParentAsSubclass.)
     *  These will not appear as lowSymbol.
     */
    private[this] val visited = HashSet[ScopeEntry]("visited", 64)

    /** Initialization has to run now so decls is populated before
     *  the declaration of curEntry.
     */
    init()

    // The current low and high symbols; the high may be null.
    private[this] var lowSymbol: Symbol  = _
    private[this] var highSymbol: Symbol = _
    def lowMemberType: Type = {
      if (lowSymbol ne lowMemberTypeCacheSym) {
        lowMemberTypeCache = self.memberType(lowSymbol)
        lowMemberTypeCacheSym = lowSymbol
      }
      lowMemberTypeCache
    }

    private[this] var lowMemberTypeCache: Type = _
    private[this] var lowMemberTypeCacheSym: Symbol = _

    // The current entry candidates for low and high symbol.
    private[this] var curEntry  = decls.elems
    private[this] var nextEntry = curEntry

    // These fields are initially populated with a call to next().
    next()

    // populate the above data structures
    private def init(): Unit = {
      // Fill `decls` with lower symbols shadowing higher ones
      def fillDecls(bcs: List[Symbol], deferred: Boolean): Unit = {
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

      // first, deferred (this will need to change if we change lookup rules!)
      fillDecls(bases, deferred = true)
      // then, concrete.
      fillDecls(bases, deferred = false)
    }

    // We can only draw conclusions about linearisation from a non-trait parent; skip Object, being the top of the lattice.
    protected lazy val nonTraitParent: Symbol =
      base.info.firstParent.typeSymbol.filter(sym => !sym.isTrait && sym != definitions.ObjectClass)

    @tailrec private def advanceNextEntry(): Unit = {
      if (nextEntry ne null) {
        nextEntry = decls lookupNextEntry nextEntry
        if (nextEntry ne null) {
          val high    = nextEntry.sym
          val isMatch = matches(high) && { visited addEntry nextEntry ; true } // side-effect visited on all matches

          // Advance if no match, or if the particular cursor is not interested in this pair
          if (!isMatch || skipOwnerPair(low.owner, high.owner)) advanceNextEntry()
          else highSymbol = high
        }
      }
    }
    @tailrec private def advanceCurEntry(): Unit = {
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
    def iterator: Iterator[SymbolPair] = new collection.AbstractIterator[SymbolPair] {
      def hasNext = cursor.hasNext
      def next()  = try cursor.currentPair finally cursor.next()
    }

    // Note that next is called once during object initialization to
    // populate the fields tracking the current symbol pair.
    @tailrec
    final def next(): Unit = {
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
