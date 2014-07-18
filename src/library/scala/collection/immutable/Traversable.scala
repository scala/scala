/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package immutable

import generic._
import mutable.Builder

/** A trait for traversable collections that are guaranteed immutable.
 *  $traversableInfo
 *  @define mutability immutable
 */
trait Traversable[+A] extends scala.collection.Traversable[A]
//                         with GenTraversable[A]
                         with GenericTraversableTemplate[A, Traversable]
                         with TraversableLike[A, Traversable[A]]
                         with Immutable {
  override def companion: GenericCompanion[Traversable] = Traversable
  override def seq: Traversable[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `List`.
 *  @define coll immutable traversable collection
 *  @define Coll `immutable.Traversable`
 */
object Traversable extends TraversableFactory[Traversable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Traversable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Traversable[A]] = new mutable.ListBuffer
}
