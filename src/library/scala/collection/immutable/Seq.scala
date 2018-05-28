package scala
package collection
package immutable

import scala.language.higherKinds

trait Seq[+A] extends Iterable[A]
                 with collection.Seq[A]
                 with SeqOps[A, Seq, Seq[A]] {

  override final def toSeq: this.type = this

  override def iterableFactory: SeqFactory[IterableCC] = Seq
}

/**
  * @define coll immutable sequence
  * @define Coll `immutable.Seq`
  */
trait SeqOps[+A, +CC[_], +C] extends Any with collection.SeqOps[A, CC, C]

/**
  * $factoryInfo
  * @define coll immutable sequence
  * @define Coll `immutable.Seq`
  */
@SerialVersionUID(3L)
object Seq extends SeqFactory.Delegate[Seq](List)

/** Base trait for immutable indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A]
                        with collection.IndexedSeq[A]
                        with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]] {

  final override def toIndexedSeq: IndexedSeq[A] = this

  override def iterableFactory: SeqFactory[IterableCC] = IndexedSeq
}

@SerialVersionUID(3L)
object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](Vector)

/** Base trait for immutable indexed Seq operations */
trait IndexedSeqOps[+A, +CC[_], +C]
  extends SeqOps[A, CC, C]
    with collection.IndexedSeqOps[A, CC, C] {

  override def slice(from: Int, until: Int): C = {
    // since we are immutable we can just share the same collection
    if (from <= 0 && until >= length) coll
    else super.slice(from, until)
  }

}

/** Base trait for immutable linear sequences that have efficient `head` and `tail` */
trait LinearSeq[+A]
  extends Seq[A]
    with collection.LinearSeq[A]
    with LinearSeqOps[A, LinearSeq, LinearSeq[A]] {

  override def iterableFactory: SeqFactory[IterableCC] = LinearSeq
}

@SerialVersionUID(3L)
object LinearSeq extends SeqFactory.Delegate[LinearSeq](List)

trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] with LinearSeqOps[A, CC, C]]
  extends Any with SeqOps[A, CC, C]
    with collection.LinearSeqOps[A, CC, C]

/** Explicit instantiation of the `Seq` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractSeq[+A] extends scala.collection.AbstractSeq[A] with Seq[A]
