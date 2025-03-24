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

package scala.collection

import scala.language.implicitConversions
import scala.collection.generic.IsSeq

object Searching {

  /** The result of performing a search on a sorted sequence
    *
    * Example usage:
    *
    * {{{
    *   val list = List(1, 3, 4, 5) // list must be sorted before searching
    *   list.search(4) // Found(2)
    *   list.search(2) // InsertionPoint(1)
    * }}}
    *
    * */
  sealed abstract class SearchResult {
    /** The index corresponding to the element searched for in the sequence, if it was found,
      * or the index where the element would be inserted in the sequence, if it was not in the sequence */
    def insertionPoint: Int
  }

  /** The result of performing a search on a sorted sequence, where the element was found.
    *
    * @param foundIndex the index corresponding to the element searched for in the sequence
    */
  case class Found(foundIndex: Int) extends SearchResult {
    override def insertionPoint: Int = foundIndex
  }

  /** The result of performing a search on a sorted sequence, where the element was not found
    *
    * @param insertionPoint the index where the element would be inserted in the sequence
    */
  case class InsertionPoint(insertionPoint: Int) extends SearchResult

  @deprecated("Search methods are defined directly on SeqOps and do not require scala.collection.Searching any more", "2.13.0")
  class SearchImpl[Repr, A](private val coll: SeqOps[A, AnyConstr, _]) extends AnyVal

  @deprecated("Search methods are defined directly on SeqOps and do not require scala.collection.Searching any more", "2.13.0")
  implicit def search[Repr, A](coll: Repr)(implicit fr: IsSeq[Repr]): SearchImpl[Repr, fr.A] =
    new SearchImpl(fr.conversion(coll))
}
