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

package scala.collection
package generic

import scala.reflect.ClassTag

/** Type class witnessing that a collection representation type `Repr` has
  * elements of type `A` and has a conversion to `SeqOps[A, Iterable, C]`, for
  * some types `A` and `C`.
  *
  * This type enables simple enrichment of `Seq`s with extension methods which
  * can make full use of the mechanics of the Scala collections framework in
  * their implementation.
  *
  * @see [[scala.collection.generic.IsIterable]]
  */
trait IsSeq[Repr] extends IsIterable[Repr] {

  @deprecated("'conversion' is now a method named 'apply'", "2.13.0")
  override val conversion: Repr => SeqOps[A, Iterable, C] = apply(_)

  /** A conversion from the type `Repr` to `SeqOps[A, Iterable, C]`
    *
    * @note The second type parameter of the returned `SeqOps` value is
    *       still `Iterable` (and not `Seq`) because `SeqView[A]` only
    *       extends `SeqOps[A, View, View[A]]`.
    */
  def apply(coll: Repr): SeqOps[A, Iterable, C]
}

object IsSeq {

  private val seqOpsIsSeqVal: IsSeq[Seq[Any]] =
    new IsSeq[Seq[Any]] {
      type A = Any
      type C = Any
      def apply(coll: Seq[Any]): SeqOps[Any, Iterable, Any] = coll
    }

  implicit def seqOpsIsSeq[CC0[X] <: SeqOps[X, Iterable, CC0[X]], A0]: IsSeq[CC0[A0]] { type A = A0; type C = CC0[A0] } =
    seqOpsIsSeqVal.asInstanceOf[IsSeq[CC0[A0]] { type A = A0; type C = CC0[A0] }]

  implicit def seqViewIsSeq[CC0[X] <: SeqView[X], A0]: IsSeq[CC0[A0]] { type A = A0; type C = View[A0] } =
    new IsSeq[CC0[A0]] {
      type A = A0
      type C = View[A]
      def apply(coll: CC0[A0]): SeqOps[A0, View, View[A0]] = coll
    }

  implicit val stringIsSeq: IsSeq[String] { type A = Char; type C = String } =
    new IsSeq[String] {
      type A = Char
      type C = String
      def apply(s: String): SeqOps[Char, immutable.IndexedSeq, String] =
        new SeqOps[Char, immutable.ArraySeq, String] {
          def length: Int = s.length
          def apply(i: Int): Char = s.charAt(i)
          def toIterable: Iterable[Char] = new immutable.WrappedString(s)
          protected[this] def coll: String = s
          protected[this] def fromSpecific(coll: IterableOnce[Char]): String = coll.iterator.mkString
          def iterableFactory: IterableFactory[immutable.ArraySeq] = immutable.ArraySeq.untagged
          override def empty: String = ""
          protected[this] def newSpecificBuilder: mutable.Builder[Char, String] = new StringBuilder
          def iterator: Iterator[Char] = s.iterator
        }
    }

  implicit val stringViewIsSeq: IsSeq[StringView] { type A = Char; type C = View[Char] } =
    new IsSeq[StringView] {
      type A = Char
      type C = View[Char]
      def apply(coll: StringView): SeqOps[Char, View, View[Char]] = coll
    }

  implicit def arrayIsSeq[A0 : ClassTag]: IsSeq[Array[A0]] { type A = A0; type C = Array[A0] } =
    new IsSeq[Array[A0]] {
      type A = A0
      type C = Array[A0]
      def apply(a: Array[A0]): SeqOps[A0, Seq, Array[A0]] =
        new SeqOps[A, mutable.ArraySeq, Array[A]] {
          def apply(i: Int): A = a(i)
          def length: Int = a.length
          def toIterable: Iterable[A] = mutable.ArraySeq.make(a)
          protected def coll: Array[A] = a
          protected def fromSpecific(coll: IterableOnce[A]): Array[A] = Array.from(coll)
          def iterableFactory: IterableFactory[mutable.ArraySeq] = mutable.ArraySeq.untagged
          override def empty: Array[A] = Array.empty[A]
          protected def newSpecificBuilder: mutable.Builder[A, Array[A]] = Array.newBuilder
          def iterator: Iterator[A] = a.iterator
        }
    }

  // `Range` can not be unified with the `CC0` parameter of the
  // `seqOpsIsSeq` definition because it does not take a type parameter.
  // Hence the need for a separate case:
  implicit def rangeIsSeq[C0 <: Range]: IsSeq[C0] { type A = Int; type C = immutable.IndexedSeq[Int] } =
    new IsSeq[C0] {
      type A = Int
      type C = immutable.IndexedSeq[Int]
      def apply(coll: C0): SeqOps[Int, Seq, immutable.IndexedSeq[Int]] = coll
    }

}
