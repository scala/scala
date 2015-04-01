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

import mutable.Builder
import scala.annotation.migration
import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds

/** A template class for companion objects of ``regular`` collection classes
 *  that represent an unconstrained higher-kinded type.
 *
 *  @tparam  A    The type of the collection elements.
 *  @tparam  CC   The type constructor representing the collection class.
 *  @author Martin Odersky
 *  @since 2.8
 *  @define coll  collection
 *  @define Coll  Traversable
 */
trait GenericTraversableTemplate[+A, +CC[X] <: GenTraversable[X]] extends HasNewBuilder[A, CC[A] @uncheckedVariance] {

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
   *
   *  @return  the first element of this $coll.
   *  @throws NoSuchElementException if the $coll is empty.
   */
  def head: A

  /** Tests whether this $coll is empty.
   *
   *  @return    `true` if the $coll contain no elements, `false` otherwise.
   */
  def isEmpty: Boolean

  /** The factory companion object that builds instances of class $Coll.
   *  (or its `Iterable` superclass where class $Coll is not a `Seq`.)
   */
  def companion: GenericCompanion[CC]

  /** The builder that builds instances of type $Coll[A]
   */
  protected[this] def newBuilder: Builder[A, CC[A]] = companion.newBuilder[A]

  /** The generic builder that builds instances of $Coll
   *  at arbitrary element types.
   */
  def genericBuilder[B]: Builder[B, CC[B]] = companion.newBuilder[B]

  private def sequential: TraversableOnce[A] = this.asInstanceOf[GenTraversableOnce[A]].seq

  /** Converts this $coll of pairs into two collections of the first and second
   *  half of each pair.
   *
   *    {{{
   *    val xs = $Coll(
   *               (1, "one"),
   *               (2, "two"),
   *               (3, "three")).unzip
   *    // xs == ($Coll(1, 2, 3),
   *    //        $Coll(one, two, three))
   *    }}}
   *
   *  @tparam A1    the type of the first half of the element pairs
   *  @tparam A2    the type of the second half of the element pairs
   *  @param asPair an implicit conversion which asserts that the element type
   *                of this $coll is a pair.
   *  @return       a pair of ${coll}s, containing the first, respectively second
   *                half of each element pair of this $coll.
   */
  def unzip[A1, A2](implicit asPair: A => (A1, A2)): (CC[A1], CC[A2]) = {
    val b1 = genericBuilder[A1]
    val b2 = genericBuilder[A2]
    for (xy <- sequential) {
      val (x, y) = asPair(xy)
      b1 += x
      b2 += y
    }
    (b1.result(), b2.result())
  }

  /** Converts this $coll of triples into three collections of the first, second,
   *  and third element of each triple.
   *
   *    {{{
   *    val xs = $Coll(
   *               (1, "one", '1'),
   *               (2, "two", '2'),
   *               (3, "three", '3')).unzip3
   *    // xs == ($Coll(1, 2, 3),
   *    //        $Coll(one, two, three),
   *    //        $Coll(1, 2, 3))
   *    }}}
   *
   *  @tparam A1       the type of the first member of the element triples
   *  @tparam A2       the type of the second member of the element triples
   *  @tparam A3       the type of the third member of the element triples
   *  @param asTriple  an implicit conversion which asserts that the element type
   *                   of this $coll is a triple.
   *  @return          a triple of ${coll}s, containing the first, second, respectively
   *                   third member of each element triple of this $coll.
   */
  def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (CC[A1], CC[A2], CC[A3]) = {
    val b1 = genericBuilder[A1]
    val b2 = genericBuilder[A2]
    val b3 = genericBuilder[A3]

    for (xyz <- sequential) {
      val (x, y, z) = asTriple(xyz)
      b1 += x
      b2 += y
      b3 += z
    }
    (b1.result(), b2.result(), b3.result())
  }

  /** Converts this $coll of traversable collections into
   *  a $coll formed by the elements of these traversable
   *  collections.
   *
   *  @tparam B the type of the elements of each traversable collection.
   *  @param asTraversable an implicit conversion which asserts that the element
   *          type of this $coll is a `GenTraversable`.
   *  @return a new $coll resulting from concatenating all element ${coll}s.
   *
   *  @usecase def flatten[B]: $Coll[B]
   *
   *    @inheritdoc
   *
   *    The resulting collection's type will be guided by the
   *    static type of $coll. For example:
   *
   *    {{{
   *    val xs = List(
   *               Set(1, 2, 3),
   *               Set(1, 2, 3)
   *             ).flatten
   *    // xs == List(1, 2, 3, 1, 2, 3)
   *
   *    val ys = Set(
   *               List(1, 2, 3),
   *               List(3, 2, 1)
   *             ).flatten
   *    // ys == Set(1, 2, 3)
   *    }}}
   */
  def flatten[B](implicit asTraversable: A => /*<:<!!!*/ GenTraversableOnce[B]): CC[B] = {
    val b = genericBuilder[B]
    for (xs <- sequential)
      b ++= asTraversable(xs).seq
    b.result()
  }

  /** Transposes this $coll of traversable collections into
   *  a $coll of ${coll}s.
   *
   *    The resulting collection's type will be guided by the
   *    static type of $coll. For example:
   *
   *    {{{
   *    val xs = List(
   *               Set(1, 2, 3),
   *               Set(4, 5, 6)).transpose
   *    // xs == List(
   *    //         List(1, 4),
   *    //         List(2, 5),
   *    //         List(3, 6))
   *
   *    val ys = Vector(
   *               List(1, 2, 3),
   *               List(4, 5, 6)).transpose
   *    // ys == Vector(
   *    //         Vector(1, 4),
   *    //         Vector(2, 5),
   *    //         Vector(3, 6))
   *    }}}
   *
   *  @tparam B the type of the elements of each traversable collection.
   *  @param  asTraversable an implicit conversion which asserts that the
   *          element type of this $coll is a `Traversable`.
   *  @return a two-dimensional $coll of ${coll}s which has as ''n''th row
   *          the ''n''th column of this $coll.
   *  @throws IllegalArgumentException if all collections in this $coll
   *          are not of the same size.
   */
  @migration("`transpose` throws an `IllegalArgumentException` if collections are not uniformly sized.", "2.9.0")
  def transpose[B](implicit asTraversable: A => /*<:<!!!*/ GenTraversableOnce[B]): CC[CC[B] @uncheckedVariance] = {
    if (isEmpty)
      return genericBuilder[CC[B]].result()

    def fail = throw new IllegalArgumentException("transpose requires all collections have the same size")

    val headSize = asTraversable(head).size
    val bs: IndexedSeq[Builder[B, CC[B]]] = IndexedSeq.fill(headSize)(genericBuilder[B])
    for (xs <- sequential) {
      var i = 0
      for (x <- asTraversable(xs).seq) {
        if (i >= headSize) fail
        bs(i) += x
        i += 1
      }
      if (i != headSize)
        fail
    }
    val bb = genericBuilder[CC[B]]
    for (b <- bs) bb += b.result
    bb.result()
  }
}

