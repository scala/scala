package scala.collection
package generic


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
      val conversion: String => SeqOps[Char, Seq, String] = s => new SeqOps[Char, Seq, String] {
        def length: Int = s.length
        def apply(i: Int): Char = s.charAt(i)
        def toIterable: Iterable[Char] = new immutable.WrappedString(s)
        protected[this] def coll: String = s
        protected[this] def fromSpecific(coll: IterableOnce[Char]): String = coll.mkString
        def iterableFactory: IterableFactory[Seq] = Seq
        protected[this] def newSpecificBuilder: mutable.Builder[Char, String] = new StringBuilder
        def iterator: Iterator[Char] = s.iterator
      }
    }

  implicit def SeqRepr[C[X] <: Seq[X], A0](implicit conv: C[A0] => SeqOps[A0, C, C[A0]]): IsSeqLike[C[A0]] { type A = A0 } =
    new IsSeqLike[C[A0]] {
      type A = A0
      val conversion = conv
    }
}
