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
import parallel.immutable.ParIterable

/** A base trait for iterable collections that are guaranteed immutable.
 *  $iterableInfo
 *
 *  @define Coll `immutable.Iterable`
 *  @define coll immutable iterable collection
 */
trait Iterable[+A] extends Traversable[A]
//                      with GenIterable[A]
                      with scala.collection.Iterable[A]
                      with GenericTraversableTemplate[A, Iterable]
                      with IterableLike[A, Iterable[A]]
                      with Parallelizable[A, ParIterable[A]]
{
  override def companion: GenericCompanion[Iterable] = Iterable
  protected[this] override def parCombiner = ParIterable.newCombiner[A] // if `immutable.IterableLike` gets introduced, please move this there!
  override def seq: Iterable[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `List`.
 *  @define Coll `immutable.Iterable`
 *  @define coll immutable iterable collection
 */
object Iterable extends TraversableFactory[Iterable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Iterable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Iterable[A]] = new mutable.ListBuffer
}
