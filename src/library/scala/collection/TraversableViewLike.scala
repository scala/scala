/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import generic._
import mutable.{ Builder, ArrayBuffer }
import TraversableView.NoBuilder
import annotation.migration

trait ViewMkString[+A] {
  self: Traversable[A] =>

  protected[this] def thisSeq: Seq[A] = new ArrayBuffer[A] ++= self result

  // Have to overload all three to work around #4299.  The overload
  // is because mkString should force a view but toString should not.
  override def mkString: String = mkString("")
  override def mkString(sep: String): String = mkString("", sep, "")
  override def mkString(start: String, sep: String, end: String): String = {
    thisSeq.addString(new StringBuilder(), start, sep, end).toString
  }
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    b append start append "..." append end
}

/** A template trait for non-strict views of traversable collections.
 *  $traversableViewInfo
 *
 *  Implementation note: Methods such as `map` or `flatMap` on this view will not invoke the implicitly passed
 *  `Builder` factory, but will return a new view directly, to preserve by-name behavior.
 *  The new view is then cast to the factory's result type. This means that every `CanBuildFrom`
 *  that takes a `View` as its `From` type parameter must yield the same view (or a generic
 *  superclass of it) as its result parameter. If that assumption is broken, cast errors might result.
 *
 * @define viewInfo
 *  A view is a lazy version of some collection. Collection transformers such as
 *  `map` or `filter` or `++` do not traverse any elements when applied on a view.
 *  Instead they create a new view which simply records that fact that the operation
 *  needs to be applied. The collection elements are accessed, and the view operations are applied,
 *  when a non-view result is needed, or when the `force` method is called on a view.
 * @define traversableViewInfo
 *  $viewInfo
 *
 *  All views for traversable collections are defined by creating a new `foreach` method.

 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @tparam A    the element type of the view
 *  @tparam Coll the type of the underlying collection containing the elements.
 *  @tparam This the type of the view itself
 */
trait TraversableViewLike[+A,
                          +Coll,
                          +This <: TraversableView[A, Coll] with TraversableViewLike[A, Coll, This]]
  extends Traversable[A] with TraversableLike[A, This] with ViewMkString[A]
{
  self =>

  override protected[this] def newBuilder: Builder[A, This] =
    throw new UnsupportedOperationException(this+".newBuilder")

  protected def underlying: Coll
  protected[this] def viewIdentifier: String = ""
  protected[this] def viewIdString: String = ""
  override def stringPrefix = "TraversableView"
  def viewToString = stringPrefix + viewIdString + "(...)"

  def force[B >: A, That](implicit bf: CanBuildFrom[Coll, B, That]) = {
    val b = bf(underlying)
    b ++= this
    b.result()
  }

  /** The implementation base trait of this view.
   *  This trait and all its subtraits has to be re-implemented for each
   *  ViewLike class.
   */
  trait Transformed[+B] extends TraversableView[B, Coll] {
    def foreach[U](f: B => U): Unit

    lazy val underlying = self.underlying
    final override protected[this] def viewIdString = self.viewIdString + viewIdentifier
    override def stringPrefix = self.stringPrefix
    override def toString = viewToString
  }
  trait EmptyView extends Transformed[Nothing] {
    final override def isEmpty = true
    final override def foreach[U](f: Nothing => U): Unit = ()
  }

  /** A fall back which forces everything into a vector and then applies an operation
   *  on it. Used for those operations which do not naturally lend themselves to a view
   */
  trait Forced[B] extends Transformed[B] {
    protected[this] val forced: Seq[B]
    def foreach[U](f: B => U) = forced foreach f
    final override protected[this] def viewIdentifier = "C"
  }

  trait Sliced extends Transformed[A] {
    protected[this] val endpoints: SliceInterval
    protected[this] def from  = endpoints.from
    protected[this] def until = endpoints.until
    // protected def newSliced(_endpoints: SliceInterval): Transformed[A] =
    //   self.newSliced(endpoints.recalculate(_endpoints))

    def foreach[U](f: A => U) {
      var index = 0
      for (x <- self) {
        if (from <= index) {
          if (until <= index) return
          f(x)
        }
        index += 1
      }
    }
    final override protected[this] def viewIdentifier = "S"
  }

  trait Mapped[B] extends Transformed[B] {
    protected[this] val mapping: A => B
    def foreach[U](f: B => U) {
      for (x <- self)
        f(mapping(x))
    }
    final override protected[this] def viewIdentifier = "M"
  }

  trait FlatMapped[B] extends Transformed[B] {
    protected[this] val mapping: A => TraversableOnce[B]
    def foreach[U](f: B => U) {
      for (x <- self)
        for (y <- mapping(x))
          f(y)
    }
    final override protected[this] def viewIdentifier = "N"
  }

  trait Appended[B >: A] extends Transformed[B] {
    protected[this] val rest: Traversable[B]
    def foreach[U](f: B => U) {
      self foreach f
      rest foreach f
    }
    final override protected[this] def viewIdentifier = "A"
  }

  trait Filtered extends Transformed[A] {
    protected[this] val pred: A => Boolean
    def foreach[U](f: A => U) {
      for (x <- self)
        if (pred(x)) f(x)
    }
    final override protected[this] def viewIdentifier = "F"
  }

  trait TakenWhile extends Transformed[A] {
    protected[this] val pred: A => Boolean
    def foreach[U](f: A => U) {
      for (x <- self) {
        if (!pred(x)) return
        f(x)
      }
    }
    final override protected[this] def viewIdentifier = "T"
  }

  trait DroppedWhile extends Transformed[A] {
    protected[this] val pred: A => Boolean
    def foreach[U](f: A => U) {
      var go = false
      for (x <- self) {
        if (!go && !pred(x)) go = true
        if (go) f(x)
      }
    }
    final override protected[this] def viewIdentifier = "D"
  }

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected def newForced[B](xs: => Seq[B]): Transformed[B] = new { val forced = xs } with Forced[B]
  protected def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new { val rest = that } with Appended[B]
  protected def newMapped[B](f: A => B): Transformed[B] = new { val mapping = f } with Mapped[B]
  protected def newFlatMapped[B](f: A => TraversableOnce[B]): Transformed[B] = new { val mapping = f } with FlatMapped[B]
  protected def newFiltered(p: A => Boolean): Transformed[A] = new { val pred = p } with Filtered
  protected def newSliced(_endpoints: SliceInterval): Transformed[A] = new { val endpoints = _endpoints } with Sliced
  protected def newDroppedWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with DroppedWhile
  protected def newTakenWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with TakenWhile

  protected def newTaken(n: Int): Transformed[A] = newSliced(SliceInterval(0, n))
  protected def newDropped(n: Int): Transformed[A] = newSliced(SliceInterval(n, Int.MaxValue))

  override def ++[B >: A, That](xs: TraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]): That = {
    newAppended(xs.toTraversable).asInstanceOf[That]
// was:    if (bf.isInstanceOf[ByPassCanBuildFrom]) newAppended(that).asInstanceOf[That]
//         else super.++[B, That](that)(bf)
  }

  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[This, B, That]): That = {
    newMapped(f).asInstanceOf[That]
//    val b = bf(repr)
//          if (b.isInstanceOf[NoBuilder[_]]) newMapped(f).asInstanceOf[That]
//    else super.map[B, That](f)(bf)
  }

  override def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[This, B, That]): That =
    filter(pf.isDefinedAt).map(pf)(bf)

  override def flatMap[B, That](f: A => TraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]): That = {
    newFlatMapped(f).asInstanceOf[That]
// was:    val b = bf(repr)
//     if (b.isInstanceOf[NoBuilder[_]]) newFlatMapped(f).asInstanceOf[That]
//    else super.flatMap[B, That](f)(bf)
  }
  private[this] implicit def asThis(xs: Transformed[A]): This = xs.asInstanceOf[This]

  override def filter(p: A => Boolean): This = newFiltered(p)
  override def withFilter(p: A => Boolean): This = newFiltered(p)
  override def partition(p: A => Boolean): (This, This) = (newFiltered(p), newFiltered(!p(_)))
  override def init: This = newSliced(SliceInterval(0, size - 1)) // !!! can't call size here.
  override def drop(n: Int): This = newDropped(n)
  override def take(n: Int): This = newTaken(n)
  override def slice(from: Int, until: Int): This = newSliced(SliceInterval(from, until))
  override def dropWhile(p: A => Boolean): This = newDroppedWhile(p)
  override def takeWhile(p: A => Boolean): This = newTakenWhile(p)
  override def span(p: A => Boolean): (This, This) = (newTakenWhile(p), newDroppedWhile(p))
  override def splitAt(n: Int): (This, This) = (newTaken(n), newDropped(n))

  override def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[This, B, That]): That =
    newForced(self.toSeq.scanLeft(z)(op)).asInstanceOf[That]

  @migration(2, 9,
    "This scanRight definition has changed in 2.9.\n" +
    "The previous behavior can be reproduced with scanRight.reverse."
  )
  override def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[This, B, That]): That =
    newForced(self.toSeq.scanRight(z)(op)).asInstanceOf[That]

  override def groupBy[K](f: A => K): immutable.Map[K, This] =
    self.toSeq.groupBy(f).mapValues(xs => newForced(xs).asInstanceOf[This])

  override def toString = viewToString
}


