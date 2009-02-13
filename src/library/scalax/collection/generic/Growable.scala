/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scalax.collection.generic

/** This class represents collections that can be augmented using a += operator.
 *
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Growable[A] {

  /** Adds a single element to this collection.
   *
   *  @param elem  the element to add.
   */
  def +=(elem: A): Unit

  /** Adds two or more elements to this collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  def +=(elem1: A, elem2: A, elems: A*) {
    this += elem1
    this += elem2
    this ++= elems.asInstanceOf[Iterable[A]] // !@!
  }

  /** Adds a number of elements provided by an iterator to this collection.
   *
   *  @param iter  the iterator.
   */
  def ++=(iter: collection.Iterator[A]) { iter foreach += }

  /** Adds a number of elements provided by an iterable object to this collection.
   *
   *  @param iter  the iterable object.
   */
  def ++=(iter: collection.Iterable[A]) { iter foreach += }

  /** Clears the collection contents.
   */
  def clear()
}




