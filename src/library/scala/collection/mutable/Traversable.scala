/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** A subtrait of <code>collection.Traversable</code> which represents
 *  traversables that can be mutated.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait Traversable[A] extends scala.collection.Traversable[A]
                        with GenericTraversableTemplate[A, Traversable]
                        with TraversableLike[A, Traversable[A]]
                        with Mutable {
  override def companion: GenericCompanion[Traversable] = Traversable
}

/** A factory object for the trait <code>Traversable</code>.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
object Traversable extends TraversableFactory[Traversable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Traversable[A]] =
    new GenericCanBuildFrom[A] {
      def apply() = newBuilder[A]
    }
  def newBuilder[A]: Builder[A, Traversable[A]] = new ArrayBuffer
}


