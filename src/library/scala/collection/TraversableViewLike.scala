/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.{Builder, ArrayBuffer}
import TraversableView.NoBuilder

/** <p>
 *    A template trait for views of <a href="../Traversable.html"
 *    target="contentFrame"><code>Traversable</code></a>.<br/>
 *    Every subclass has to implement the <code>foreach</code> method.
 *  </p>
 *  @note Methods such as map/flatMap on this will not invoke the implicitly passed
 *        Builder factory, but will return a new view directly, to preserve by-name behavior.
 *        The new view is then cast to the factory's result type.
 *        This means that every CanBuildFrom that takes a
 *        View as its From type parameter must yield the same view (or a generic superclass of it)
 *        as its result parameter. If that assumption is broken, cast errors might result.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait TraversableViewLike[+A,
                          +Coll,
                          +This <: TraversableView[A, Coll] with TraversableViewLike[A, Coll, This]]
  extends Traversable[A] with TraversableLike[A, This] {
self =>

  override protected[this] def newBuilder: Builder[A, This] =
    throw new UnsupportedOperationException(this+".newBuilder")

  protected def underlying: Coll

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
    lazy val underlying = self.underlying
    override def toString = stringPrefix+"(...)"
  }

  /** A fall back which forces everything into a vector and then applies an operation
   *  on it. Used for those operations which do not naturally lend themselves to a view
   */
  trait Forced[B] extends Transformed[B] {
    protected[this] def forced: Seq[B]
    private[this] lazy val forcedCache = forced
    override def foreach[U](f: B => U) = forcedCache.foreach(f)
    override def stringPrefix = self.stringPrefix+"C"
  }

  /** pre: from >= 0
   */
  trait Sliced extends Transformed[A] {
    protected[this] val from: Int
    protected[this] val until: Int
    override def foreach[U](f: A => U) {
      var index = 0
      for (x <- self) {
        if (from <= index) {
          if (until <= index) return
          f(x)
        }
        index += 1
      }
    }
    override def stringPrefix = self.stringPrefix+"S"
    override def slice(from1: Int, until1: Int): This =
      newSliced(from1 max 0, until1 max 0).asInstanceOf[This]
  }

  trait Mapped[B] extends Transformed[B] {
    protected[this] val mapping: A => B
    override def foreach[U](f: B => U) {
      for (x <- self)
        f(mapping(x))
    }
    override def stringPrefix = self.stringPrefix+"M"
  }

  trait FlatMapped[B] extends Transformed[B] {
    protected[this] val mapping: A => Traversable[B]
    override def foreach[U](f: B => U) {
      for (x <- self)
        for (y <- mapping(x))
          f(y)
    }
    override def stringPrefix = self.stringPrefix+"N"
  }

  trait Appended[B >: A] extends Transformed[B] {
    protected[this] val rest: Traversable[B]
    override def foreach[U](f: B => U) {
      for (x <- self) f(x)
      for (x <- rest) f(x)
    }
    override def stringPrefix = self.stringPrefix+"A"
  }

  trait Filtered extends Transformed[A] {
    protected[this] val pred: A => Boolean
    override def foreach[U](f: A => U) {
      for (x <- self)
        if (pred(x)) f(x)
    }
    override def stringPrefix = self.stringPrefix+"F"
  }

  trait TakenWhile extends Transformed[A] {
    protected[this] val pred: A => Boolean
    override def foreach[U](f: A => U) {
      for (x <- self) {
        if (!pred(x)) return
        f(x)
      }
    }
    override def stringPrefix = self.stringPrefix+"T"
  }

  trait DroppedWhile extends Transformed[A] {
    protected[this] val pred: A => Boolean
    override def foreach[U](f: A => U) {
      var go = false
      for (x <- self) {
        if (!go && !pred(x)) go = true
        if (go) f(x)
      }
    }
    override def stringPrefix = self.stringPrefix+"D"
  }

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected def newForced[B](xs: => Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
  protected def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }

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

  override def flatMap[B, That](f: A => Traversable[B])(implicit bf: CanBuildFrom[This, B, That]): That = {
    newFlatMapped(f).asInstanceOf[That]
// was:    val b = bf(repr)
//     if (b.isInstanceOf[NoBuilder[_]]) newFlatMapped(f).asInstanceOf[That]
//    else super.flatMap[B, That](f)(bf)
  }

  protected[this] def thisSeq: Seq[A] = {
    val buf = new ArrayBuffer[A]
    self foreach (buf +=)
    buf.result
  }

  override def filter(p: A => Boolean): This = newFiltered(p).asInstanceOf[This]
  override def partition(p: A => Boolean): (This, This) = (filter(p), filter(!p(_)))
  override def init: This = newSliced(0, size - 1).asInstanceOf[This]
  override def drop(n: Int): This = newSliced(n max 0, Int.MaxValue).asInstanceOf[This]
  override def take(n: Int): This = newSliced(0, n).asInstanceOf[This]
  override def slice(from: Int, until: Int): This = newSliced(from max 0, until).asInstanceOf[This]
  override def dropWhile(p: A => Boolean): This = newDroppedWhile(p).asInstanceOf[This]
  override def takeWhile(p: A => Boolean): This = newTakenWhile(p).asInstanceOf[This]
  override def span(p: A => Boolean): (This, This) = (takeWhile(p), dropWhile(p))
  override def splitAt(n: Int): (This, This) = (take(n), drop(n))

  override def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[This, B, That]): That =
    newForced(thisSeq.scanLeft(z)(op)).asInstanceOf[That]

  override def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[This, B, That]): That =
    newForced(thisSeq.scanRight(z)(op)).asInstanceOf[That]

  override def groupBy[K](f: A => K): Map[K, This] =
    thisSeq.groupBy(f).mapValues(xs => newForced(xs).asInstanceOf[This])

  override def stringPrefix = "TraversableView"
}


