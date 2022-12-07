/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package generic

import scala.collection.immutable.{HashSet, TreeMap, TreeSet}


/** This trait represents collection-like objects that can be reduced
 *  using a '+' operator. It defines variants of `-` and `--`
 *  as convenience methods in terms of single-element removal `-`.
 *  @tparam   A    the type of the elements of the $coll.
 *  @tparam   Repr the type of the $coll itself
 *  @author   Martin Odersky
 *  @since    2.8
 *  @define   coll collection
 *  @define   Coll Subtractable
 */
trait Subtractable[A, +Repr <: Subtractable[A, Repr]] { self =>

  /** The representation object of type `Repr` which contains the collection's elements
   */
  protected def repr: Repr

  /** Creates a new $coll from this $coll with an element removed.
   *  @param elem the element to remove
   *  @return a new collection that contains all elements of the current $coll
   *  except one less occurrence of `elem`.
   */
  def -(elem: A): Repr

  /** Creates a new $coll from this $coll with some elements removed.
   *
   *  This method takes two or more elements to be removed. Another overloaded
   *  variant of this method handles the case where a single element is
   *  removed.
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @return a new $coll that contains all elements of the current $coll
   *  except one less occurrence of each of the given elements.
   */
  def -(elem1: A, elem2: A, elems: A*): Repr =
    this - elem1 - elem2 -- elems

  /** Creates a new $coll from this $coll by removing all elements of another
   *  collection.
   *
   *  @param xs     the collection containing the removed elements.
   *  @return a new $coll that contains all elements of the current $coll
   *  except one less occurrence of each of the elements of `elems`.
   */
  def --(xs: GenTraversableOnce[A]): Repr = this match {
    case hs: HashSet[A] if xs.isInstanceOf[HashSet[A]] =>
      hs.diff(xs.asInstanceOf[HashSet[A]]).asInstanceOf[Repr]
    case ts: TreeMap[A, _] =>
      ts.removeAllImpl(xs).asInstanceOf[Repr]
    case ts: TreeSet[A]  =>
      ts.removeAll(xs).asInstanceOf[Repr]
    case _ =>
      xs.seq.foldLeft(repr) (_ - _)
  }
}
