/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

import scala.collection.mutable.{ Builder, GrowingBuilder }
import scala.language.higherKinds

/**
 * @define Coll `mutable.SortedSet`
 * @define coll mutable sorted set
 *
 * @author Lucien Pereira
 *
 */
abstract class MutableSortedSetFactory[CC[A] <: mutable.SortedSet[A] with SortedSetLike[A, CC[A]] with mutable.Set[A] with mutable.SetLike[A, CC[A]]] extends SortedSetFactory[CC] {

  /**
   * mutable.SetBuilder uses '+' which is not a primitive for anything extending mutable.SetLike,
   * this causes serious performance issues since each time 'elems = elems + x'
   * is evaluated elems is cloned (which is O(n)).
   *
   * Fortunately GrowingBuilder comes to rescue.
   *
   */
  override def newBuilder[A](implicit ord: Ordering[A]): Builder[A, CC[A]] = new GrowingBuilder[A, CC[A]](empty)

}
