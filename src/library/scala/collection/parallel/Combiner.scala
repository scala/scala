/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel

import scala.collection.Parallel
import scala.collection.mutable.Builder
import scala.collection.generic.Sizing

/** The base trait for all combiners.
 *  A combiner incremental collection construction just like
 *  a regular builder, but also implements an efficient merge operation of two builders
 *  via `combine` method. Once the collection is constructed, it may be obtained by invoking
 *  the `result` method.
 *
 *  The complexity of the `combine` method should be less than linear for best
 *  performance. The `result` method doesn't have to be a constant time operation,
 *  but may be performed in parallel.
 *
 *  @tparam Elem   the type of the elements added to the builder
 *  @tparam To     the type of the collection the builder produces
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait Combiner[-Elem, +To] extends Builder[Elem, To] with Sizing with Parallel {

  @transient
  @volatile
  var _combinerTaskSupport = defaultTaskSupport

  def combinerTaskSupport = {
    val cts = _combinerTaskSupport
    if (cts eq null) {
      _combinerTaskSupport = defaultTaskSupport
      defaultTaskSupport
    } else cts
  }

  def combinerTaskSupport_=(cts: TaskSupport) = _combinerTaskSupport = cts

  /** Combines the contents of the receiver builder and the `other` builder,
   *  producing a new builder containing both their elements.
   *
   *  This method may combine the two builders by copying them into a larger collection,
   *  by producing a lazy view that gets evaluated once `result` is invoked, or use
   *  a merge operation specific to the data structure in question.
   *
   *  Note that both the receiver builder and `other` builder become invalidated
   *  after the invocation of this method, and should be cleared (see `clear`)
   *  if they are to be used again.
   *
   *  Also, combining two combiners `c1` and `c2` for which `c1 eq c2` is `true`, that is,
   *  they are the same objects in memory:
   *
   *  {{{
   *  c1.combine(c2)
   *  }}}
   *
   *  always does nothing and returns `c1`.
   *
   *  @tparam N      the type of elements contained by the `other` builder
   *  @tparam NewTo  the type of collection produced by the `other` builder
   *  @param other   the other builder
   *  @return        the parallel builder containing both the elements of this and the `other` builder
   */
  def combine[N <: Elem, NewTo >: To](other: Combiner[N, NewTo]): Combiner[N, NewTo]

  /** Returns `true` if this combiner has a thread-safe `+=` and is meant to be shared
   *  across several threads constructing the collection.
   *
   *  By default, this method returns `false`.
   */
  def canBeShared: Boolean = false

  /** Constructs the result and sets the appropriate tasksupport object to the resulting collection
   *  if this is applicable.
   */
  def resultWithTaskSupport: To = {
    val res = result()
    setTaskSupport(res, combinerTaskSupport)
  }
}

/*
private[collection] trait EnvironmentPassingCombiner[-Elem, +To] extends Combiner[Elem, To] {
  abstract override def result = {
    val res = super.result
    res
  }
}
*/
