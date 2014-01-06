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

import scala.annotation.tailrec

/** This trait forms part of collections that can be augmented
 *  using a `+=` operator and that can be cleared of all elements using
 *  a `clear` method.
 *
 *  @author   Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define coll growable collection
 *  @define Coll `Growable`
 *  @define add  add
 *  @define Add  add
 */
trait Growable[-A] extends Clearable {

  /** ${Add}s a single element to this $coll.
   *
   *  @param elem  the element to $add.
   *  @return the $coll itself
   */
  def +=(elem: A): this.type

  /** ${Add}s two or more elements to this $coll.
   *
   *  @param elem1 the first element to $add.
   *  @param elem2 the second element to $add.
   *  @param elems the remaining elements to $add.
   *  @return the $coll itself
   */
  def +=(elem1: A, elem2: A, elems: A*): this.type = this += elem1 += elem2 ++= elems

  /** ${Add}s all elements produced by a TraversableOnce to this $coll.
   *
   *  @param xs   the TraversableOnce producing the elements to $add.
   *  @return  the $coll itself.
   */
  def ++=(xs: TraversableOnce[A]): this.type = {
    @tailrec def loop(xs: scala.collection.LinearSeq[A]) {
      if (xs.nonEmpty) {
        this += xs.head
        loop(xs.tail)
      }
    }
    xs match {
      case xs: scala.collection.LinearSeq[_] => loop(xs)
      case xs                                => xs foreach +=
    }
    this
  }

  /** Clears the $coll's contents. After this operation, the
   *  $coll is empty.
   */
  def clear(): Unit
}
