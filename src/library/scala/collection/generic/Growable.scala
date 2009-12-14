/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package generic

/** This trait forms part of collections that can be augmented
 *  using a `+=` operator and that can be cleared of all elements using
 *  a `clear` method.
 *
 *  @author   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define coll growable collection
 *  @define Coll Growable
 *  @define add  add
 *  @define Add  add
 */
trait Growable[-A] {

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

  /** ${Add}s all elements produced by an iterator to this $coll.
   *
   *  @param iter  the iterator producing the elements to $add.
   *  @return  the $coll itself.
   */
  def ++=(iter: Iterator[A]): this.type = { iter foreach += ; this }

  /** ${Add}s all elements contained in a traversable collection to this $coll.
   *
   *  @param elems  the collection containing the elements to $add.
   *  @return  the $coll itself.
   */
  def ++=(elems: Traversable[A]): this.type = { elems foreach +=; this }

  /** Clears the $coll's contents. After this operation, the
   *  $coll is empty.
   */
  def clear()
}




