/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.Builder

/** A trait for traversable collections that are guaranteed immutable.
 *  $traversableInfo
 *  @define mutability immutable
 */
trait Traversable[+A] extends scala.collection.Traversable[A]
                         with GenericTraversableTemplate[A, Traversable]
                         with TraversableLike[A, Traversable[A]]
                         with Immutable {
  override def companion: GenericCompanion[Traversable] = Traversable
}

/** $factoryInfo
 *  @define Coll immutable.Travesable
 *  @define coll immutable traversable object
 */
object Traversable extends TraversableFactory[Traversable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Traversable[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, Traversable[A]] = new mutable.ListBuffer
}
