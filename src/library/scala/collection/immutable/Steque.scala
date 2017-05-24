/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

import generic._
import mutable.{Builder, ListBuffer}
import scala.annotation.tailrec

/**
  * Stack-ended catenable queue. Supports O(1) append, and (amortized)
  * O(1) `uncons`, such that walking the sequence via N successive `uncons`
  * steps takes O(N). Like a difference list, conversion to a `Seq[A]`
  * takes linear time, regardless of how the sequence is built up.
  * Conversion from a `Seq` takes constant time, but maintaining `uncons`
  * performance in that case depends on the underlying `Seq`'s `uncons` performance.
  *
  * Implementation from fs2.util.Catenable in the Functional Streams for Scala (fs2) project
  */
sealed abstract class Steque[+A]
  extends AbstractSeq[A]
    with LinearSeq[A]
    with GenericTraversableTemplate[A, Steque]
    with LinearSeqOptimized[A, Steque[A]]
    with Serializable {

  import Steque._

  override final def companion: GenericCompanion[Steque] = Steque

  /** Returns the head and tail of this catenable if non empty, none otherwise. Amortized O(1). */
  @inline
  final def uncons[B >: Null <: AnyRef](default: B, ifPresent: (A, Steque[A]) => B): B = {
    var c: Steque[A] = this
    val rights = new collection.mutable.ArrayBuffer[Steque[A]]
    while (true) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            return default
          } else {
            c = rights.last
            rights.remove(rights.length - 1)
          }
        case OfSeq(underlyingSequence) =>
          val next =
            rights.foldLeft(Steque.fromSeq(underlyingSequence.tail))((x, y) => Append(y, x))
          return ifPresent(underlyingSequence.head, next)
        case Single(a) =>
          val next = if (rights.isEmpty) empty else rights.reduceLeft((x, y) => Append(y, x))
          return ifPresent(a, next)
        case Append(l, r) => c = l; rights += r
      }
    }
    ???
  }

  final override def tail: Steque[A] = {
    var c: Steque[A] = this
    val rights = new collection.mutable.ArrayBuffer[Steque[A]]
    while (true) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            throw new NoSuchElementException("tail on empty steque")
          } else {
            c = rights.last
            rights.remove(rights.length - 1)
          }
        case Single(_) =>
          val next = if (rights.isEmpty) empty else rights.reduceLeft((x, y) => Append(y, x))
          return next
        case OfSeq(underlyingSequence) =>
          val next =
            if (rights.isEmpty) Steque.fromSeq(underlyingSequence.tail)
            else Steque.fromSeq(underlyingSequence.tail) ++ rights.reduceLeft((x, y) => Append(y, x))
          return next
        case Append(l, r) =>
          c = l
          rights += r
      }
    }
    ???
  }

  final override def head: A = {
    var c: Steque[A] = this
    val rights = new collection.mutable.ArrayBuffer[Steque[A]]
    while (true) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            throw new NoSuchElementException("head on empty steque")
          } else {
            c = rights.last
            rights.remove(rights.length - 1)
          }
        case Single(a) =>
          return a
        case OfSeq(underlyingSequence) =>
          return underlyingSequence.head
        case Append(l, r) =>
          c = l
          rights += r
      }
    }
    ???
  }

  final override def ++[B >: A, That](c: GenTraversableOnce[B])(implicit cbf: CanBuildFrom[Steque[A], B, That]): That = {
    if (cbf eq Steque.ReusableCBF) {
      append(this, c.asInstanceOf[Steque[A]]).asInstanceOf[That]
    } else {
      super.++[B, That](c)
    }
  }

  /** Returns a new catenable consisting of `a` followed by this. O(1) runtime. */
  final def cons[A2 >: A](a: A2): Steque[A2] =
    if (this eq Empty) single(a)
    else Append(single(a), this)

  /** Alias for [[cons]]. */
  final def +:[A2 >: A](a: A2): Steque[A2] =
    cons(a)

  /** Returns a new catenable consisting of this followed by `a`. O(1) runtime. */
  final def snoc[A2 >: A](a: A2): Steque[A2] =
    if (this eq Empty) single(a)
    else Append(this, single(a))

  /** Alias for [[snoc]]. */
  final def :+[A2 >: A](a: A2): Steque[A2] =
    snoc(a)

  /** Applies the supplied function to each element and returns a new catenable. */
  override final def map[B, That](f: A => B)(implicit cbf: CanBuildFrom[Steque[A], B, That]): That = {
    if (cbf eq Steque.ReusableCBF) {
      foldLeft(empty[B])((acc, a) => acc :+ f(a)).asInstanceOf[That]
    } else {
      super.map(f)
    }
  }

  override final def apply(idx: Int): A = {
    var counter: Int = 0
    foreachHalting { a =>
      val willHalt = idx == counter
      if (willHalt) {
        return a
        false
      } else {
        counter += 1
        true
      }
    }
    throw new IndexOutOfBoundsException()
  }

  // may be possible to optimize, by not touching the back.
  override final def updated[AA >: A, That](idx: Int, elem: AA)(implicit cbf: CanBuildFrom[Steque[A], AA, That]): That = {
    val bldr = cbf.apply()
    var counter: Int = 0
    foreach { a => bldr += (if (idx == counter) elem else a); counter += 1 }
    bldr.result()
  }

  override final def length: Int = {
    var len = 0
    foreach(_ => len += 1)
    len
  }

  /** Folds over the elements from left to right using the supplied initial value and function. */
  override final def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var result = z
    foreach(a => result = f(result, a))
    result
  }

  @inline
  private def foreachHalting(f: A => Boolean): Unit = {
    var c: Steque[A] = this
    val rights = new collection.mutable.ArrayBuffer[Steque[A]]
    while (c ne null) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            c = null
          } else {
            c = rights.last
            rights.remove(rights.length - 1)
          }
        case Single(a) =>
          val continue = f(a)
          if (continue) {
            c = if (rights.isEmpty) Empty else rights.reduceLeft((x, y) => Append(y, x))
            rights.clear()
          } else {
            c = null
          }
        case OfSeq(underlyingSequence) =>
          underlyingSequence.reverse.foldRight(true)((a, b) => b && f(a))
          if (rights.isEmpty) {
            c = null
          } else {
            c = rights.last
            rights.remove(rights.length - 1)
          }
        case Append(l, r) => c = l; rights += r
      }
    }
  }

  /** Applies the supplied function to each element, left to right. */
  override final def foreach[U](f: A => U): Unit = {
    foreachHalting(f.andThen(_ => true))
  }

  /** Converts to a list. */
  override final def toList: List[A] = {
    val builder = List.newBuilder[A]
    foreach { a => builder += a; () }
    builder.result()
  }

  override final def toString = {
    if (this eq Empty) {
      "Steque()"
    } else {
      val sb = new StringBuilder("Steque(")
      foreach { a => sb ++= a.toString; sb ++= ", " }
      sb.setCharAt(sb.length - 2, ')')
      sb.deleteCharAt(sb.length - 1)
      sb.result()
    }
  }
}

object Steque extends SeqFactory[Steque] {

  final case object Empty extends Steque[Nothing] {
    override def isEmpty: Boolean = true
  }

  final case class Single[A](a: A) extends Steque[A] {
    override def isEmpty: Boolean = false
  }

  final case class Append[A](left: Steque[A], right: Steque[A]) extends Steque[A] {
    override def isEmpty: Boolean = false // b/c `append` constructor doesn't allow either branch to be empty
  }

  final case class OfSeq[A](underlyingSequence: collection.Seq[A]) extends Steque[A] {
    override def isEmpty: Boolean = false // b/c `ofSeq` constructor doesn't allow the underlying sequence to be empty
  }

  override def newBuilder[A]: mutable.Builder[A, Steque[A]] = new mutable.Builder[A, Steque[A]] {
    var current: Steque[A] = empty

    override def +=(elem: A): this.type = {
      current = current.snoc(elem)
      this
    }

    override def clear(): Unit = {
      current = empty
    }

    override def result(): Steque[A] = {
      current
    }
  }

  /** Empty catenable. */
  override def empty[A]: Steque[A] = Empty

  /** Creates a catenable of 1 element. */
  def single[A](a: A): Steque[A] = Single(a)

  /** Appends two catenables. */
  def append[A](c: Steque[A], c2: Steque[A]): Steque[A] =
    if (c.isEmpty) {
      if (c2.isEmpty)
        Empty
      else
        c2
    } else if (c2.isEmpty) {
      c
    } else {
      Append(c, c2)
    }

  /** Creates a catenable from the specified sequence. */
  def fromSeq[A](s: collection.Seq[A]): Steque[A] =
    if (s.isEmpty) Empty
    else OfSeq(s)

  /** Creates a catenable from the specified elements. */
  override def apply[A](as: A*): Steque[A] = {
    fromSeq(as)
  }

  implicit final def canBuildFrom[A]: CanBuildFrom[Coll, A, Steque[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

}

