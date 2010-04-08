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

import mutable.Builder
import annotation.unchecked.uncheckedVariance

/** A template class for companion objects of ``regular'' collection classes
 *  that represent an unconstrained higher-kinded type.
 *  @tparam  A    The type of the collection elements.
 *  @tparam  CC   The type constructor representing the collection class.
 *  @author Martin Odersky
 *  @since 2.8
 *  @define coll  collection
 *  @define Coll  CC
 */
trait GenericTraversableTemplate[+A, +CC[X] <: Traversable[X]] extends HasNewBuilder[A, CC[A] @uncheckedVariance] {

  /** Applies a function `f` to all elements of this $coll.
   *
   *  @param  f   the function that is applied for its side-effect to every element.
   *              The result of function `f` is discarded.
   *
   *  @tparam  U  the type parameter describing the result of function `f`.
   *              This result will always be ignored. Typically `U` is `Unit`,
   *              but this is not necessary.
   *
   *  @usecase def foreach(f: A => Unit): Unit
   */
  def foreach[U](f: A => U): Unit

  /** Selects the first element of this $coll.
   *  @return  the first element of this $coll.
   *  @throws `NoSuchElementException` if the $coll is empty.
   */
  def head: A

  /** Tests whether this $coll is empty.
   *
   *  @return    `true` if the $coll contain no elements, `false` otherwise.
   */
  def isEmpty: Boolean

  /** The factory companion object that builds instances of class $Coll.
   */
  def companion: GenericCompanion[CC]

  /** The builder that builds instances of type $Coll[A]
   */
  protected[this] def newBuilder: Builder[A, CC[A]] = companion.newBuilder[A]

  /** The generic builder that builds instances of $Coll
   *  at arbitrary element types.
   */
  def genericBuilder[B]: Builder[B, CC[B]] = companion.newBuilder[B]

  /** Converts this $coll of pairs into two collections of the first and second
   *  halfs of each pair.
   *  @param A1 the type of the first half of the element pairs
   *  @param A2 the type of the second half of the element pairs
   *  @param asPair an implicit conversion which asserts that the element type of this
   *          $coll is a pair.
   *  @return a pair ${coll}s, containing the first, respectively second half
   *          of each element pair of this $coll.
   */
  def unzip[A1, A2](implicit asPair: A => /*<:<!!!*/ (A1, A2)): (CC[A1], CC[A2]) = {
    val b1 = genericBuilder[A1]
    val b2 = genericBuilder[A2]
    for (xy <- this) {
      val (x, y) = asPair(xy)
      b1 += x
      b2 += y
    }
    (b1.result, b2.result)
  }

  /** Converts this $coll of traversable collections into
   *  a $coll in which all element collections are concatenated.
   *  @tparam B the type of the elements of each traversable collection.
   *  @param asTraversable an implicit conversion which asserts that the element type of this
   *         $coll is a `Traversable`.
   *  @return a new $coll resulting from concatenating all element ${coll}s.
   *  @usecase def flatten[B]: $Coll[B]
   */
  def flatten[B](implicit asTraversable: A => /*<:<!!!*/ Traversable[B]): CC[B] = {
    val b = genericBuilder[B]
    for (xs <- this)
      b ++= asTraversable(xs)
    b.result
  }

  /** Transposes this $coll of traversable collections into
   *  a $coll of ${coll}s.
   *  @tparam B the type of the elements of each traversable collection.
   *  @param  asTraversable an implicit conversion which asserts that the element type of this
   *          $coll is a `Traversable`.
   *  @return a two-dimensional $coll of ${coll}s which has as ''n''th row
   *          the ''n''th column of this $coll.
   */
  def transpose[B](implicit asTraversable: A => /*<:<!!!*/ Traversable[B]): CC[CC[B] @uncheckedVariance] = {
    val bs: IndexedSeq[Builder[B, CC[B]]] = asTraversable(head).map(_ => genericBuilder[B]).toIndexedSeq
    for (xs <- this) {
      var i = 0
      for (x <- asTraversable(xs)) {
        bs(i) += x
        i += 1
      }
    }
    val bb = genericBuilder[CC[B]]
    for (b <- bs) bb += b.result
    bb.result
  }
}

