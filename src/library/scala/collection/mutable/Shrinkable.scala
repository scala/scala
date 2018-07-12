package scala
package collection.mutable

import scala.annotation.tailrec

/** This trait forms part of collections that can be reduced
  *  using a `-=` operator.
  *
  *  @author   Martin Odersky
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
  def -= (elem: A): this.type

  /** Alias for `-=` */
  @`inline` final def subtractOne(elem: A): this.type = this -= elem

  /** Removes two or more elements from this $coll.
    *
    *  @param elem1 the first element to remove.
    *  @param elem2 the second element to remove.
    *  @param elems the remaining elements to remove.
    *  @return the $coll itself
    */
  @deprecated("Use --= with a collection instead of -= with varargs", "2.13.0")
  def -= (elem1: A, elem2: A, elems: A*): this.type = {
    this -= elem1
    this -= elem2
    this --= elems
  }

  /** Removes all elements produced by an iterator from this $coll.
    *
    *  @param xs   the iterator producing the elements to remove.
    *  @return the $coll itself
    */
  def --= (xs: collection.IterableOnce[A]): this.type = {
    @tailrec def loop(xs: collection.LinearSeq[A]): Unit = {
      if (xs.nonEmpty) {
        subtractOne(xs.head)
        loop(xs.tail)
      }
    }
    xs match {
      case xs: collection.LinearSeq[A] => loop(xs)
      case xs => xs.iterator.foreach(subtractOne)
    }
    this
  }

  /** Alias for `--=` */
  @`inline` final def subtractAll(xs: collection.IterableOnce[A]): this.type = this --= xs

}
