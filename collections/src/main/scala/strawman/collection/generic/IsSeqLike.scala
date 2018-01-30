package strawman.collection
package generic

import scala.{Any, Char}
import scala.Predef.{String, implicitly}

/** Type class witnessing that a collection representation type `Repr` has
  * elements of type `A` and has a conversion to `SeqOps[A, Seq, Repr]`.
  *
  * This type enables simple enrichment of `Seq`s with extension methods which
  * can make full use of the mechanics of the Scala collections framework in
  * their implementation.
  *
  * @see [[scala.collection.generic.IsIterableLike]]
  */
trait IsSeqLike[Repr] {
  /** The type of elements we can traverse over. */
  type A
  /** A conversion from the representation type `Repr` to `SeqOps[A, Seq, Repr]`. */
  val conversion: Repr => SeqOps[A, Seq, Repr]
}

object IsSeqLike {
  import scala.language.higherKinds

  implicit val stringRepr: IsSeqLike[String] { type A = Char } =
    new IsSeqLike[String] {
      type A = Char
      val conversion = implicitly[String => SeqOps[Char, Seq, String]]
    }

  implicit def SeqRepr[C[X] <: Seq[X], A0](implicit conv: C[A0] => SeqOps[A0, C, C[A0]]): IsSeqLike[C[A0]] { type A = A0 } =
    new IsSeqLike[C[A0]] {
      type A = A0
      val conversion = conv
    }
}
