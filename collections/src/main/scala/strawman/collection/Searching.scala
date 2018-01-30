/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package strawman.collection

import scala.{Int, deprecated, AnyVal}
import scala.language.implicitConversions
import scala.math.Ordering
import strawman.collection.generic.IsSeqLike

object Searching {
  sealed abstract class SearchResult {
    def insertionPoint: Int
  }

  case class Found(foundIndex: Int) extends SearchResult {
    override def insertionPoint = foundIndex
  }

  case class InsertionPoint(insertionPoint: Int) extends SearchResult

  @deprecated("Search methods are defined directly on SeqOps and do not require scala.collection.Searching any more", "2.13.0")
  class SearchImpl[A, Repr](private val coll: SeqOps[A, Seq, Repr]) extends AnyVal

  @deprecated("Search methods are defined directly on SeqOps and do not require scala.collection.Searching any more", "2.13.0")
  implicit def search[Repr, A](coll: Repr)(implicit fr: IsSeqLike[Repr]): SearchImpl[fr.A, Repr] =
    new SearchImpl(fr.conversion(coll))
}
