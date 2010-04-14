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

/** A base trait for iterable collections that are guaranteed immutable.
 *  $iterableInfo
 *
 *  @define Coll immutable.Iterable
 *  @define coll immutable iterable collection
 */
trait Iterable[+A] extends Traversable[A]
                      with scala.collection.Iterable[A]
                      with GenericTraversableTemplate[A, Iterable]
                      with IterableLike[A, Iterable[A]] {
  override def companion: GenericCompanion[Iterable] = Iterable
}

/** $factoryInfo
 *  @define Coll immutable.Iterable
 *  @define coll immutable iterable collection
 */
object Iterable extends TraversableFactory[Iterable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Iterable[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, Iterable[A]] = new mutable.ListBuffer
}
