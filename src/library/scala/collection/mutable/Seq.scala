package scala.collection.mutable

import scala.collection.SeqFactory
import scala.language.higherKinds

trait Seq[A]
  extends Iterable[A]
    with collection.Seq[A]
    with SeqOps[A, Seq, Seq[A]] {

  override def iterableFactory: SeqFactory[IterableCC] = Seq
}

/**
  * $factoryInfo
  * @define coll mutable sequence
  * @define Coll `mutable.Seq`
  */
@SerialVersionUID(3L)
object Seq extends SeqFactory.Delegate[Seq](ArrayBuffer)

/**
  * @define coll mutable sequence
  * @define Coll `mutable.Seq`
  */
trait SeqOps[A, +CC[_], +C <: AnyRef]
  extends collection.SeqOps[A, CC, C]
    with Cloneable[C] {

  override def clone(): C = {
    val b = newSpecificBuilder
    b ++= toIterable
    b.result()
  }

  /** Replaces element at given index with a new value.
    *
    *  @param idx      the index of the element to replace.
    *  @param elem     the new value.
    *  @throws   IndexOutOfBoundsException if the index is not valid.
    */
  @throws[IndexOutOfBoundsException]
  def update(idx: Int, elem: A): Unit

  @deprecated("Use `mapInPlace` on an `IndexedSeq` instead", "2.13.0")
  @`inline`final def transform(f: A => A): this.type = {
    var i = 0
    val siz = size
    while (i < siz) { this(i) = f(this(i)); i += 1 }
    this
  }
}

/** Explicit instantiation of the `Seq` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractSeq[A] extends scala.collection.AbstractSeq[A] with Seq[A]
