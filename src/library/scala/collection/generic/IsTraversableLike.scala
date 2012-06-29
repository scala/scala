/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

/** Type class witnessing that a collection representation type `Repr` has
 *  elements of type `A` and has a conversion to `GenTraversableLike[A, Repr]`.
 *
 *  This type enables simple enrichment of `GenTraversable`s with extension
 *  methods which can make full use of the mechanics of the Scala collections
 *  framework in their implementation.
 *
 *  Example usage,
 * {{{
 *    class FilterMapImpl[A, Repr](val r: GenTraversableLike[A, Repr]) {
 *      final def filterMap[B, That](f: A => Option[B])(implicit cbf: CanBuildFrom[Repr, B, That]): That =
 *        r.flatMap(f(_).toSeq)
 *    }
 *    implicit def filterMap[Repr, A](r: Repr)(implicit fr: IsTraversableOnce[Repr]): FilterMapImpl[fr.A,Repr] =
 *      new FilterMapImpl(fr.conversion(r))
 *
 *    val l = List(1, 2, 3, 4, 5)
 *    List(1, 2, 3, 4, 5) filterMap (i => if(i % 2 == 0) Some(i) else None)
 *    // == List(2, 4)
 * }}}
 *
 * @author Miles Sabin
 * @author J. Suereth
 * @since 2.10
 */
trait IsTraversableLike[Repr] {
  /** The type of elements we can traverse over. */
  type A
  /** A conversion from the representation type `Repr` to a `GenTraversableLike[A,Repr]`. */
  val conversion: Repr => GenTraversableLike[A, Repr]
}

object IsTraversableLike {
  import language.higherKinds

  implicit val stringRepr: IsTraversableLike[String] { type A = Char } =
    new IsTraversableLike[String] {
      type A = Char
      val conversion = implicitly[String => GenTraversableLike[Char, String]]
    }

  implicit def genTraversableLikeRepr[C[_], A0](implicit conv: C[A0] => GenTraversableLike[A0,C[A0]]): IsTraversableLike[C[A0]] { type A = A0 } = 
    new IsTraversableLike[C[A0]] {
      type A = A0
      val conversion = conv
    }
}
