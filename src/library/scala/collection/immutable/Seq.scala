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

package scala
package collection
package immutable

trait Seq[+A] extends Iterable[A]
                 with collection.Seq[A]
                 with SeqOps[A, Seq, Seq[A]]
                 with IterableFactoryDefaults[A, Seq] {

  override final def toSeq: this.type = this

  override def iterableFactory: SeqFactory[Seq] = Seq
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
object Seq extends SeqFactory.Delegate[Seq](List) {
  override def from[E](it: IterableOnce[E]): Seq[E] = it match {
    case s: Seq[E] => s
    case _ => super.from(it)
  }
}

/** Base trait for immutable indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A]
                        with collection.IndexedSeq[A]
                        with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]
                        with IterableFactoryDefaults[A, IndexedSeq] {

  final override def toIndexedSeq: IndexedSeq[A] = this

  override def canEqual(that: Any): Boolean = that match {
    case otherIndexedSeq: IndexedSeq[_] => length == otherIndexedSeq.length && super.canEqual(that)
    case _ => super.canEqual(that)
  }


  override def sameElements[B >: A](o: IterableOnce[B]): Boolean = o match {
    case that: IndexedSeq[_] =>
      (this eq that) || {
        val length = this.length
        var equal = length == that.length
        if (equal) {
          var index = 0
          // some IndexedSeq apply is less efficient than using Iterators
          // e.g. Vector so we can compare the first few with apply and the rest with an iterator
          // but if apply is more efficient than Iterators then we can use the apply for all the comparison
          // we default to the minimum preferred length
          val maxApplyCompare = {
            val preferredLength = Math.min(applyPreferredMaxLength, that.applyPreferredMaxLength)
            if (length > (preferredLength.toLong << 1)) preferredLength else length
          }
          while (index < maxApplyCompare && equal) {
            equal = this (index) == that(index)
            index += 1
          }
          if ((index < length) && equal) {
            val thisIt = this.iterator.drop(index)
            val thatIt = that.iterator.drop(index)
            while (equal && thisIt.hasNext) {
              equal = thisIt.next() == thatIt.next()
            }
          }
        }
        equal
      }
    case _ => super.sameElements(o)
  }

  /** a hint to the runtime when scanning values
    * [[apply]] is preferred for scan with a max index less than this value
    * [[iterator]] is preferred for scans above this range
    * @return a hint about when to use [[apply]] or [[iterator]]
    */
  protected def applyPreferredMaxLength: Int = IndexedSeqDefaults.defaultApplyPreferredMaxLength

  override def iterableFactory: SeqFactory[IndexedSeq] = IndexedSeq
}

object IndexedSeqDefaults {
  val defaultApplyPreferredMaxLength: Int =
    try System.getProperty(
      "scala.collection.immutable.IndexedSeq.defaultApplyPreferredMaxLength", "64").toInt
    catch {
      case _: SecurityException => 64
    }
}

@SerialVersionUID(3L)
object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](Vector) {
  override def from[E](it: IterableOnce[E]): IndexedSeq[E] = it match {
    case is: IndexedSeq[E] => is
    case _ => super.from(it)
  }
}

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
    with LinearSeqOps[A, LinearSeq, LinearSeq[A]]
    with IterableFactoryDefaults[A, LinearSeq] {

  override def iterableFactory: SeqFactory[LinearSeq] = LinearSeq
}

@SerialVersionUID(3L)
object LinearSeq extends SeqFactory.Delegate[LinearSeq](List) {
  override def from[E](it: IterableOnce[E]): LinearSeq[E] = it match {
    case ls: LinearSeq[E] => ls
    case _ => super.from(it)
  }
}

trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] with LinearSeqOps[A, CC, C]]
  extends Any with SeqOps[A, CC, C]
    with collection.LinearSeqOps[A, CC, C]

/** Explicit instantiation of the `Seq` trait to reduce class file size in subclasses. */
abstract class AbstractSeq[+A] extends scala.collection.AbstractSeq[A] with Seq[A]
