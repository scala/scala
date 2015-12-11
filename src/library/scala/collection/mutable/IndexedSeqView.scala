/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import generic._

import TraversableView.NoBuilder
import scala.language.implicitConversions

/** A non-strict view of a mutable `IndexedSeq`.
 *  $viewInfo
 *  Some of the operations of this class will yield again a mutable indexed sequence,
 *  others will just yield a plain indexed sequence of type `collection.IndexedSeq`.
 *  Because this is a leaf class there is no associated `Like` class.
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @tparam A    the element type of the view
 *  @tparam Coll the type of the underlying collection containing the elements.
 */
trait IndexedSeqView[A, +Coll] extends IndexedSeq[A]
                                  with IndexedSeqOptimized[A, IndexedSeqView[A, Coll]]
                                  with SeqView[A, Coll]
                                  with SeqViewLike[A, Coll, IndexedSeqView[A, Coll]] {
self =>

  protected[this] type This = IndexedSeqView[A, Coll]

  def update(idx: Int, elem: A): Unit

  trait TransformedX[B] extends IndexedSeqView[B, Coll] with super.TransformedS[B] {
    def update(idx: Int, elem: B): Unit
    override def toString = viewToString
  }

  /** Explicit instantiation of the `Transformed` trait to reduce class file size in subclasses. */
  private[collection] abstract class AbstractTransformedX[B] extends super.AbstractTransformedS[B] with TransformedX[B]

  // pre: until <= self.length
  trait SlicedX extends super.SlicedS with TransformedX[A] {
    override def length = endpoints.width
    def update(idx: Int, elem: A) =
      if (idx >= 0 && idx + from < until) self.update(idx + from, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait FilteredX extends super.FilteredS with TransformedX[A] {
    def update(idx: Int, elem: A) = self.update(index(idx), elem)
  }

  trait TakenWhileX extends super.TakenWhileS with TransformedX[A] {
    def update(idx: Int, elem: A) =
      if (idx < len) self.update(idx, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait DroppedWhileX extends super.DroppedWhileS with TransformedX[A] {
    def update(idx: Int, elem: A) =
      if (idx >= 0) self.update(idx + start, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait ReversedX extends super.ReversedS with TransformedX[A] {
    def update(idx: Int, elem: A) = self.update(self.length - 1 - idx, elem)
  }

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected override def newFiltered(p: A => Boolean): TransformedX[A] = new AbstractTransformedX[A] with FilteredX { lazy val pred = p }
  protected override def newSliced(_endpoints: SliceInterval): TransformedX[A] = new AbstractTransformedX[A] with SlicedX { lazy val endpoints = _endpoints }
  protected override def newDroppedWhile(p: A => Boolean): TransformedX[A] = new AbstractTransformedX[A] with DroppedWhileX { lazy val pred = p }
  protected override def newTakenWhile(p: A => Boolean): TransformedX[A] = new AbstractTransformedX[A] with TakenWhileX { lazy val pred = p }
  protected override def newReversed: TransformedX[A] = new AbstractTransformedX[A] with ReversedX

  override def filter(p: A => Boolean): This = newFiltered(p)
  override def init: This = newSliced(SliceInterval(0, self.length - 1))
  override def drop(n: Int): This = newSliced(SliceInterval(n, self.length))
  override def take(n: Int): This = newSliced(SliceInterval(0, n min self.length))
  override def slice(from: Int, until: Int): This = newSliced(SliceInterval(from, until min self.length))
  override def dropWhile(p: A => Boolean): This = newDroppedWhile(p)
  override def takeWhile(p: A => Boolean): This = newTakenWhile(p)
  override def span(p: A => Boolean): (This, This) = (newTakenWhile(p), newDroppedWhile(p))
  override def splitAt(n: Int): (This, This) = (take(n), drop(n)) // !!!
  override def reverse: This = newReversed
  override def tail: IndexedSeqView[A, Coll] = if (isEmpty) super.tail else slice(1, length)
}

/** An object containing the necessary implicit definitions to make
 *  `SeqView`s work. Its definitions are generally not accessed directly by clients.
 *
 * Note that the `canBuildFrom` factories yield `SeqView`s, not `IndexedSeqView`s.
 * This is intentional, because not all operations yield again a `mutable.IndexedSeqView`.
 * For instance, `map` just gives a `SeqView`, which reflects the fact that
 * `map` cannot do its work and maintain a pointer into the original indexed sequence.
 */
object IndexedSeqView {
  type Coll = TraversableView[_, _ <: Traversable[_]]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, SeqView[A, Seq[_]]] =
    new CanBuildFrom[Coll, A, SeqView[A, Seq[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
  implicit def arrCanBuildFrom[A]: CanBuildFrom[TraversableView[_, Array[_]], A, SeqView[A, Array[A]]] =
    new CanBuildFrom[TraversableView[_, Array[_]], A, SeqView[A, Array[A]]] {
      def apply(from: TraversableView[_, Array[_]]) = new NoBuilder
      def apply() = new NoBuilder
    }
}
