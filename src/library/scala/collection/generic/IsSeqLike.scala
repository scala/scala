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
  *  elements of type `A` and has a conversion to `SeqLike[A, Repr]`.
  *
  * @see [[scala.collection.generic.IsTraversableLike]]
  */
trait IsSeqLike[Repr] {
  /** The type of elements we can traverse over. */
  type A
  /** A conversion from the representation type `Repr` to a `SeqLike[A,Repr]`. */
  val conversion: Repr => SeqLike[A, Repr]
}

object IsSeqLike {
  import language.higherKinds

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
