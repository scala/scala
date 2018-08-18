/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
  class SearchImpl[Repr, A](private val coll: SeqOps[A, AnyConstr, _]) extends AnyVal

  @deprecated("Search methods are defined directly on SeqOps and do not require scala.collection.Searching any more", "2.13.0")
  implicit def search[Repr, A](coll: Repr)(implicit fr: IsSeq[Repr]): SearchImpl[Repr, fr.A] =
    new SearchImpl(fr.conversion(coll))
}
