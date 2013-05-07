/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel

import scala.collection.{ Seq, Iterator }

/** A splitter (or a split iterator) can be split into more splitters that traverse over
 *  disjoint subsets of elements.
 *
 *  @tparam T    type of the elements this splitter traverses
 *
 *  @since 2.9
 *  @author Aleksandar Prokopec
 */
trait Splitter[+T] extends Iterator[T] {

  /** Splits the iterator into a sequence of disjunct views.
   *
   *  Returns a sequence of split iterators, each iterating over some subset of the
   *  elements in the collection. These subsets are disjoint and should be approximately
   *  equal in size. These subsets are not empty, unless the iterator is empty in which
   *  case this method returns a sequence with a single empty iterator. If the splitter has
   *  more than two elements, this method will return two or more splitters.
   *
   *  Implementors are advised to keep this partition relatively small - two splitters are
   *  already enough when partitioning the collection, although there may be a few more.
   *
   *  '''Note:''' this method actually invalidates the current splitter.
   *
   *  @return a sequence of disjunct iterators of the collection
   */
  def split: Seq[Splitter[T]]
  /*
   *  '''Note:''' splitters in this sequence may actually be empty and it can contain a splitter
   *  which iterates over the same elements as the original splitter AS LONG AS calling `split`
   *  a finite number of times on the resulting splitters eventually returns a nontrivial partition.
   *
   *  Note that the docs contract above yields implementations which are a subset of implementations
   *  defined by this fineprint.
   *
   *  The rationale behind this is best given by the following example:
   *  try splitting an iterator over a linear hash table.
   */
}

object Splitter {
  def empty[T]: Splitter[T] = new Splitter[T] {
    def hasNext = false
    def next = Iterator.empty.next()
    def split = Seq(this)
  }
}
