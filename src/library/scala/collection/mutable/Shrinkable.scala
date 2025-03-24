/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection.mutable

import scala.annotation.tailrec

/** This trait forms part of collections that can be reduced
  *  using a `-=` operator.
  *
  *  @define coll shrinkable collection
  *  @define Coll `Shrinkable`
  */
trait Shrinkable[-A] {

  /** Removes a single element from this $coll.
    *
    *  @param elem  the element to remove.
    *  @return the $coll itself
    */
  def subtractOne(elem: A): this.type

  /** Alias for `subtractOne` */
  @`inline` final def -= (elem: A): this.type = subtractOne(elem)

  /** Removes two or more elements from this $coll.
    *
    *  @param elem1 the first element to remove.
    *  @param elem2 the second element to remove.
    *  @param elems the remaining elements to remove.
    *  @return the $coll itself
    */
  @deprecated("Use `--=` aka `subtractAll` instead of varargs `-=`; infix operations with an operand of multiple args will be deprecated", "2.13.3")
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
  def subtractAll(xs: collection.IterableOnce[A]): this.type = {
    @tailrec def loop(xs: collection.LinearSeq[A]): Unit = {
      if (xs.nonEmpty) {
        subtractOne(xs.head)
        loop(xs.tail)
      }
    }
    if (xs.asInstanceOf[AnyRef] eq this) { // avoid mutating under our own iterator
      xs match {
        case xs: Clearable => xs.clear()
        case xs            => subtractAll(Buffer.from(xs))
      }
    } else {
      xs match {
        case xs: collection.LinearSeq[A] => loop(xs)
        case xs => xs.iterator.foreach(subtractOne)
      }
    }
    this
  }

  /** Alias for `subtractAll` */
  @`inline` final def --= (xs: collection.IterableOnce[A]): this.type = subtractAll(xs)

}
