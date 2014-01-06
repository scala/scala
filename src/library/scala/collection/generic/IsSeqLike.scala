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

/** Type class witnessing that a collection representation type `Repr` has
  * elements of type `A` and has a conversion to `SeqLike[A, Repr]`.
  *
  * This type enables simple enrichment of `Seq`s with extension methods which
  * can make full use of the mechanics of the Scala collections framework in
  * their implementation.
  *
  * Example usage:
  * {{{
  *    class FilterMapImpl[A, Repr](val r: SeqLike[A, Repr]) {
  *      final def filterMap[B, That](f: A => Option[B])(implicit cbf: CanBuildFrom[Repr, B, That]): That =
  *        r.flatMap(f(_))
  *    }
  *    implicit def filterMap[Repr, A](r: Repr)(implicit fr: IsSeqLike[Repr]): FilterMapImpl[fr.A,Repr] =
  *      new FilterMapImpl(fr.conversion(r))
  *
  *    val l = List(1, 2, 3, 4, 5)
  *    List(1, 2, 3, 4, 5) filterMap (i => if(i % 2 == 0) Some(i) else None)
  *    // == List(2, 4)
  * }}}
  *
  * @see [[scala.collection.Seq]]
  * @see [[scala.collection.generic.IsTraversableLike]]
  */
trait IsSeqLike[Repr] {
  /** The type of elements we can traverse over. */
  type A
  /** A conversion from the representation type `Repr` to a `SeqLike[A,Repr]`. */
  val conversion: Repr => SeqLike[A, Repr]
}

object IsSeqLike {
  import scala.language.higherKinds

  implicit val stringRepr: IsSeqLike[String] { type A = Char } =
    new IsSeqLike[String] {
      type A = Char
      val conversion = implicitly[String => SeqLike[Char, String]]
    }

  implicit def seqLikeRepr[C[_], A0](implicit conv: C[A0] => SeqLike[A0,C[A0]]): IsSeqLike[C[A0]] { type A = A0 } =
    new IsSeqLike[C[A0]] {
      type A = A0
      val conversion = conv
    }
}
