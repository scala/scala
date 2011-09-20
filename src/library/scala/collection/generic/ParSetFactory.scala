/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.generic

import collection.mutable.Builder
import collection.parallel.Combiner
import collection.parallel.ParSet
import collection.parallel.ParSetLike

/**
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
abstract class ParSetFactory[CC[X] <: ParSet[X] with ParSetLike[X, CC[X], _] with GenericParTemplate[X, CC]]
  extends GenSetFactory[CC]
     with GenericParCompanion[CC]
{
  def newBuilder[A]: Combiner[A, CC[A]] = newCombiner[A]

  def newCombiner[A]: Combiner[A, CC[A]]

  class GenericCanCombineFrom[A] extends CanCombineFrom[CC[_], A, CC[A]] {
    override def apply(from: Coll) = from.genericCombiner[A]
    override def apply() = newCombiner[A]
  }
}

