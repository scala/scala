/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Sequence.scala 16092 2008-09-12 10:37:06Z nielsen $


package scalax.collection.generic.mutable

import collection.mutable.Vector
import collection.mutable.Vector._

/** A non-strict projection of an iterable.
 * @author Sean McDirmid
 * @author Martin Odersky
 * @note this should really be a virtual class of SequenceFactory
 */
trait VectorView[+UC[B] <: Vector[B], A] extends SequenceView[UC, A] with Vector[A] {
self =>

  /** refined from Iterable.View */
  val origin: Vector[_]

  trait Transformed[B] extends super.Transformed[B] with VectorView[UC, B] {
    override val origin = self
    override def elements: Iterator[B] = new Elements(0, length)
    override protected def asCC = asInstanceOf[VectorView[UC, B]]
  }

  class Appended(that: Vector[A]) extends super.Appended[A](that) with Transformed[A] {
    override def update(idx: Int, elem: A)  {
      val ll = self.length
      if (idx < ll) self.update(idx, elem) else that.update(idx - ll, elem)
    }
  }

  class Sliced(from: Int, to: Int) extends super.Sliced(from, to) with Transformed[A] {
    override def update(idx: Int, elem: A) {
      if (idx >= 0 && idx < length) self.update(idx + from, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
    }
    override def slice(from1: Int, to1: Int) =
      new self.Sliced(from + (from1 min length max 0) , to + (to1 min length max 0))
  }

  class Reversed extends super.Reversed with Transformed[A] {
    override def update(idx: Int, elem: A) {
      self.update(length - 1 - idx, elem)
    }
  }

  class Zipped[B](that: Vector[B]) extends super.Zipped[B](that) with Transformed[(A, B)] {
    override def update(idx: Int, elem: (A, B)) {
      self.update(idx, elem._1)
      that.update(idx, elem._2)
    }
  }

  def ++(that: Vector[A]): VectorView[UC, A] =
    new Appended(that).asCC

  override def reverse: VectorView[UC, A] =
    (new Reversed).asCC

  private def toVector[B](xs: Iterable[B]): Vector[B] = xs match {
    case ras: Vector[_] => ras.asInstanceOf[Vector[B]]
    case _ => Vector() ++ xs
  }

  override def zip[B](that: Iterable[B]): VectorView[UC, (A, B)] =
    new Zipped(toVector(that)).asCC

  override def zipWithIndex: VectorView[UC, (A, Int)] =
    zip((0 until length).asInstanceOf[Null]) // !@!
  override def take(n: Int): VectorView[UC, A] =
    slice(0, n)
  override def drop(n: Int): VectorView[UC, A] =
    slice(n, Math.MAX_INT)
  override def splitAt(n: Int): (VectorView[UC, A], VectorView[UC, A]) = (take(n), drop(n))
  override def slice(from: Int, until: Int): VectorView[UC, A] =
    new Sliced(from, until).asCC
  override def takeWhile(p: A => Boolean): VectorView[UC, A] =
    take(prefixLength(p))
  override def dropWhile(p: A => Boolean): VectorView[UC, A] =
    drop(prefixLength(p))
  override def span(p: A => Boolean): (VectorView[UC, A], VectorView[UC, A]) = {
    val n = prefixLength(p)
    (take(n), drop(n))
  }
}

