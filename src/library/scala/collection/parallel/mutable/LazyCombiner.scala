/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel.mutable

import scala.collection.generic.Growable
import scala.collection.generic.Sizing
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.Combiner

/** Implements combining contents of two combiners
 *  by postponing the operation until `result` method is called. It chains
 *  the leaf results together instead of evaluating the actual collection.
 *
 *  @tparam Elem    the type of the elements in the combiner
 *  @tparam To      the type of the collection the combiner produces
 *  @tparam Buff    the type of the buffers that contain leaf results and this combiner chains together
 */
trait LazyCombiner[Elem, +To, Buff <: Growable[Elem] with Sizing] extends Combiner[Elem, To] {
//self: scala.collection.parallel.EnvironmentPassingCombiner[Elem, To] =>
  val chain: ArrayBuffer[Buff]
  val lastbuff = chain.last
  def +=(elem: Elem) = { lastbuff += elem; this }
  def result: To = allocateAndCopy
  def clear() = { chain.clear() }
  def combine[N <: Elem, NewTo >: To](other: Combiner[N, NewTo]): Combiner[N, NewTo] = if (this ne other) {
    if (other.isInstanceOf[LazyCombiner[_, _, _]]) {
      val that = other.asInstanceOf[LazyCombiner[Elem, To, Buff]]
      newLazyCombiner(chain ++= that.chain)
    } else throw new UnsupportedOperationException("Cannot combine with combiner of different type.")
  } else this
  def size = chain.foldLeft(0)(_ + _.size)

  /** Method that allocates the data structure and copies elements into it using
   *  `size` and `chain` members.
   */
  def allocateAndCopy: To
  def newLazyCombiner(buffchain: ArrayBuffer[Buff]): LazyCombiner[Elem, To, Buff]
}
