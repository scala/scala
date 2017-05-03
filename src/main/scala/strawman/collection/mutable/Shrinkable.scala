package strawman
package collection.mutable

import scala.annotation.tailrec

import collection.toNewSeq
import scala.{`inline`, Unit}

/** This trait forms part of collections that can be reduced
  *  using a `-=` operator.
  *
  *  @author   Martin Odersky
  *  @version 2.8
  *  @since   2.8
  *  @define coll shrinkable collection
  *  @define Coll `Shrinkable`
  */
trait Shrinkable[-A] {

  /** Removes a single element from this $coll.
    *
    *  @param elem  the element to remove.
    *  @return the $coll itself
    */
  def subtract(elem: A): this.type

  /** Alias for `subtract` */
  @`inline` final def -= (elem: A): this.type = subtract(elem)

  /** Removes two or more elements from this $coll.
    *
    *  @param elem1 the first element to remove.
    *  @param elem2 the second element to remove.
    *  @param elems the remaining elements to remove.
    *  @return the $coll itself
    */
  def subtract(elem1: A, elem2: A, elems: A*): this.type = {
    this -= elem1
    this -= elem2
    this --= elems.toStrawman
  }

  /** Alias for `subtract` */
  @`inline` final def -= (elem1: A, elem2: A, elems: A*): this.type = subtract(elem1, elem2, elems: _*)

  /** Removes all elements produced by an iterator from this $coll.
    *
    *  @param xs   the iterator producing the elements to remove.
    *  @return the $coll itself
    */
  def subtractAll(xs: collection.IterableOnce[A]): this.type = {
    @tailrec def loop(xs: collection.LinearSeq[A]): Unit = {
      if (xs.nonEmpty) {
        subtract(xs.head)
        loop(xs.tail)
      }
    }
    xs match {
      case xs: collection.LinearSeq[A] => loop(xs)
      case xs => xs.iterator().foreach(subtract)
    }
    this
  }

  /** Alias for `subtractAll` */
  @`inline` final def --= (xs: collection.IterableOnce[A]): this.type = subtractAll(xs)

}