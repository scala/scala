package strawman.collection
package decorators

import scala.{Array, Char, Int}
import scala.Predef.String
import strawman.collection.immutable.{ImmutableArray, Range}

/** Type class witnessing that a collection type `C` has
  * elements of type `A` and has a conversion to `SeqOps[A, _, _]`.
  *
  * This type enables simple enrichment of `Seq`s with extension methods which
  * can make full use of the mechanics of the Scala collections framework in
  * their implementation.
  *
  * @see [[scala.collection.decorators.HasIterableOps]]
  */
trait HasSeqOps[C] extends HasIterableOps[C] {
  /** A conversion from the type `C` to `SeqOps[A, _, _]`. */
  def apply(c: C): SeqOps[A, AnyConstr, _]
}

object HasSeqOps {

  // we want to provide implicit instances that unify all possible types `X` with a `SeqOps[A, CC, C]`
  // 1. Seq collections
  implicit def seqHasSeqOps[CC[X] <: SeqOps[X, AnyConstr, _], A0]: HasSeqOps[CC[A0]] {type A = A0 } =
    new HasSeqOps[CC[A0]] {
      type A = A0
      def apply(c: CC[A0]): SeqOps[A0, AnyConstr, _] = c
    }

  // 2. String
  implicit def stringHasSeqOps: HasSeqOps[String] { type A = Char } =
    new HasSeqOps[String] {
      type A = Char
      def apply(c: String): SeqOps[Char, AnyConstr, _] = stringToStringOps(c)
    }

  // 3. StringView
  implicit def stringViewHasSeqOps: HasSeqOps[StringView] { type A = Char } =
    new HasSeqOps[StringView] {
      type A = Char
      def apply(c: StringView): SeqOps[Char, AnyConstr, _] = c
    }

  // 4. Array
  implicit def arrayHasSeqOps[A0]: HasSeqOps[Array[A0]] { type A = A0 } =
    new HasSeqOps[Array[A0]] {
      type A = A0
      def apply(c: Array[A0]): SeqOps[A0, AnyConstr, _] = ImmutableArray.unsafeWrapArray(c)
    }

  // 5. Range collections
  implicit def rangeHasSeqOps[C <: Range]: HasSeqOps[C] { type A = Int } =
    new HasSeqOps[C] {
      type A = Int
      def apply(c: C): SeqOps[Int, AnyConstr, _] = c
    }

}
