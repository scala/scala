/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

import scala.reflect.internal.SymbolPairs

/** A class that yields a kind of iterator (`Cursor`),
 *  which yields pairs of corresponding symbols visible in some base class,
 *  unless there's a parent class that already contains the same pairs.
 *  Most of the logic is in SymbolPairs, which contains generic
 *  pair-oriented traversal logic.
 */
abstract class OverridingPairs extends SymbolPairs {
  import global._

  class Cursor(base: Symbol) extends super.Cursor(base) {
    /** Symbols to exclude: Here these are constructors and private/artifact symbols,
     *  including bridges. But it may be refined in subclasses.
     */
    override protected def exclude(sym: Symbol) = (
         sym.isPrivateLocal
      || sym.isArtifact
      || sym.isConstructor
      || (sym.isPrivate && sym.owner != base) // Privates aren't inherited. Needed for pos/t7475a.scala
    )

    /** Types always match. Term symbols match if their member types
     *  relative to `self` match.
     */
    override protected def matches(high: Symbol) = low.isType || (
         (low.owner != high.owner)     // don't try to form pairs from overloaded members
      && !high.isPrivate               // private or private[this] members never are overridden
      && !exclude(low)                 // this admits private, as one can't have a private member that matches a less-private member.
      && (lowMemberType matches (self memberType high))
    ) // TODO we don't call exclude(high), should we?
  }
}
