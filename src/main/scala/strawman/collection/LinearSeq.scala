package strawman
package collection

import scala.{Any, Boolean, IndexOutOfBoundsException, Int, throws}

/** Base trait for linearly accessed sequences that have efficient `head` and
  *  `tail` operations.
  *  Known subclasses: List, LazyList
  */
trait LinearSeq[+A] extends Seq[A] with LinearSeqOps[A, LinearSeq, LinearSeq[A]]

/** Base trait for linear Seq operations */
trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A]] extends Any with SeqOps[A, CC, C] {

  /** To be overridden in implementations: */
  def isEmpty: Boolean
  def head: A
  def tail: LinearSeq[A]

  /** `iterator` is implemented in terms of `head` and `tail` */
  def iterator() = new Iterator[A] {
    private[this] var current: Iterable[A] = coll
    def hasNext = !current.isEmpty
    def next() = { val r = current.head; current = current.tail; r }
  }

  override def size: Int = {
    var these = coll
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  final def length: Int = size

  /** Optimized version of `drop` that avoids copying
    *  Note: `drop` is defined here, rather than in a trait like `LinearSeqMonoTransforms`,
    *  because the `...MonoTransforms` traits make no assumption about the type of `Repr`
    *  whereas we need to assume here that `Repr` is the same as the underlying
    *  collection type.
    */
  override def drop(n: Int): C = { // sound bcs of VarianceNote
    def loop(n: Int, s: Iterable[A]): C =
      if (n <= 0) s.asInstanceOf[C]
      // implicit contract to guarantee success of asInstanceOf:
      //   (1) coll is of type C[A]
      //   (2) The tail of a LinearSeq is of the same type as the type of the sequence itself
      // it's surprisingly tricky/ugly to turn this into actual types, so we
      // leave this contract implicit.
      else loop(n - 1, s.tail)
    loop(n, coll)
  }

  /** `apply` is defined in terms of `drop`, which is in turn defined in
    *  terms of `tail`.
    */
  @throws[IndexOutOfBoundsException]
  override def apply(n: Int): A = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val skipped = drop(n)
    if (skipped.isEmpty) throw new IndexOutOfBoundsException(n.toString)
    skipped.head
  }
}
