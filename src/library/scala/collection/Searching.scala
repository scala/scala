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

package scala.collection

import scala.language.implicitConversions
import scala.collection.generic.IsSeq

object Searching {
  sealed abstract class SearchResult {
    def insertionPoint: Int
  }

  case class Found(foundIndex: Int) extends SearchResult {
    override def insertionPoint = foundIndex
  }

  case class InsertionPoint(insertionPoint: Int) extends SearchResult

  @deprecated("Search methods are defined directly on SeqOps and do not require scala.collection.Searching any more", "2.13.0")
  class SearchImpl[Repr, A](private val coll: SeqOps[A, Iterable, _]) extends AnyVal

  @deprecated("Search methods are defined directly on SeqOps and do not require scala.collection.Searching any more", "2.13.0")
  implicit def search[Repr, A](coll: Repr)(implicit fr: IsSeq[Repr]): SearchImpl[Repr, fr.A] =
    new SearchImpl(fr.conversion(coll))
}
