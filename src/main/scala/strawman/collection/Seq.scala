package strawman.collection

import scala.{Any, Boolean, Equals, IndexOutOfBoundsException, Int}
import strawman.collection.immutable.{List, Nil}

import scala.annotation.unchecked.uncheckedVariance
import scala.util.hashing.MurmurHash3

/** Base trait for sequence collections */
trait Seq[+A] extends Iterable[A] with SeqLike[A, Seq] with ArrayLike[A]

/** Base trait for linearly accessed sequences that have efficient `head` and
  *  `tail` operations.
  *  Known subclasses: List, LazyList
  */
trait LinearSeq[+A] extends Seq[A] with LinearSeqLike[A, LinearSeq] { self =>

  /** To be overridden in implementations: */
  def isEmpty: Boolean
  def head: A
  def tail: LinearSeq[A]

  /** `iterator` is overridden in terms of `head` and `tail` */
  def iterator() = new Iterator[A] {
    private[this] var current: Seq[A] = self
    def hasNext = !current.isEmpty
    def next() = { val r = current.head; current = current.tail; r }
  }

  /** `length` is defined in terms of `iterator` */
  def length: Int = iterator().length

  /** `apply` is defined in terms of `drop`, which is in turn defined in
    *  terms of `tail`.
    */
  override def apply(n: Int): A = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val skipped = drop(n)
    if (skipped.isEmpty) throw new IndexOutOfBoundsException(n.toString)
    skipped.head
  }
}

trait IndexedSeq[+A] extends Seq[A] { self =>
  override def view: IndexedView[A] = new IndexedView[A] {
    def length: Int = self.length
    def apply(i: Int): A = self(i)
  }
}

/** Base trait for Seq operations */
trait SeqLike[+A, +C[X] <: Seq[X]]
  extends IterableLike[A, C]
    with SeqMonoTransforms[A, C[A @uncheckedVariance]] // sound bcs of VarianceNote
    with Equals {

  protected def coll: C[A @uncheckedVariance]

  /** Do the elements of this collection are the same (and in the same order)
    * as those of `that`?
    */
  def sameElements[B >: A](that: IterableOnce[B]): Boolean =
    coll.iterator().sameElements(that)

  /** Method called from equality methods, so that user-defined subclasses can
    *  refuse to be equal to other collections of the same kind.
    *  @param   that   The object with which this $coll should be compared
    *  @return  `true`, if this $coll can possibly equal `that`, `false` otherwise. The test
    *           takes into consideration only the run-time types of objects but ignores their elements.
    */
  def canEqual(that: Any): Boolean = true

  override def equals(o: scala.Any): Boolean =
    o match {
      case it: Seq[A] => (it canEqual this) && sameElements(it)
      case _ => false
    }

  override def hashCode(): Int =
    Seq.stableIterableHash(coll)

}

// Temporary: TODO move to MurmurHash3.scala
object Seq {

  final def stableIterableHash(xs: Seq[_]): Int = {
    var n = 0
    var h = "Seq".##
    val it = xs.iterator()
    while (it.hasNext) {
      h = MurmurHash3.mix(h, it.next().##)
      n += 1
    }
    MurmurHash3.finalizeHash(h, n)
  }

}

/** Base trait for linear Seq operations */
trait LinearSeqLike[+A, +C[X] <: LinearSeq[X]] extends SeqLike[A, C] {

  /** Optimized version of `drop` that avoids copying
    *  Note: `drop` is defined here, rather than in a trait like `LinearSeqMonoTransforms`,
    *  because the `...MonoTransforms` traits make no assumption about the type of `Repr`
    *  whereas we need to assume here that `Repr` is the same as the underlying
    *  collection type.
    */
  override def drop(n: Int): C[A @uncheckedVariance] = { // sound bcs of VarianceNote
  def loop(n: Int, s: Iterable[A]): C[A] =
    if (n <= 0) s.asInstanceOf[C[A]]
    // implicit contract to guarantee success of asInstanceOf:
    //   (1) coll is of type C[A]
    //   (2) The tail of a LinearSeq is of the same type as the type of the sequence itself
    // it's surprisingly tricky/ugly to turn this into actual types, so we
    // leave this contract implicit.
    else loop(n - 1, s.tail)
    loop(n, coll)
  }
}

/** Type-preserving transforms over sequences. */
trait SeqMonoTransforms[+A, +Repr] extends Any with IterableMonoTransforms[A, Repr] {
  def reverse: Repr = coll.view match {
    case v: IndexedView[A] => fromIterableWithSameElemType(v.reverse)
    case _ =>
      var xs: List[A] = Nil
      val it = coll.iterator()
      while (it.hasNext) xs = it.next() :: xs
      fromIterableWithSameElemType(xs)
  }
}

/** A trait representing indexable collections with finite length */
trait ArrayLike[+A] extends Any {
  def length: Int
  def apply(i: Int): A
}
