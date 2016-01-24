/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

import scala.collection.parallel.Combiner
import scala.collection.parallel.ParSet
import scala.collection.parallel.ParSetLike
import scala.language.higherKinds

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

