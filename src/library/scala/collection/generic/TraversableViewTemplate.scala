/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic
import scala.collection._

import Math.MAX_INT
import TraversableView.NoBuilder

/** <p>
 *    A base class for views of <a href="../Traversable.html"
 *    target="contentFrame"><code>Traversable</code></a>.<br/>
 *    Every subclass has to implement the <code>foreach</code> method.
 *  </p>
 *  @note Methods such as map/flatMap on this will not invoke the implicitly passed
 *        Builder factory, but will return a new view directly, to preserve by-name behavior.
 *        The new view is then cast to the factory's result type.
 *        This means that every BuilderFactory that takes a
 *        View as its From type parameter must yield the same view (or a generic superclass of it)
 *        as its result parameter. If that assumption is broken, cast errors might result.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait TraversableViewTemplate[+A,
                              +Coll,
                              +This <: TraversableView[A, Coll] with TraversableViewTemplate[A, Coll, This]]
  extends Traversable[A] with TraversableTemplate[A, This] {
self =>

  override protected[this] def newBuilder: Builder[A, This] =
    throw new UnsupportedOperationException(this+".newBuilder")

  protected def underlying: Coll

  def force[B >: A, That](implicit bf: BuilderFactory[B, That, Coll]) = {
    val b = bf(underlying)
    b ++= this
    b.result()
  }

  trait Transformed[+B] extends TraversableView[B, Coll] {
    lazy val underlying = self.underlying
  }

  /** pre: from >= 0
   */
  trait Sliced extends Transformed[A] {
    protected[this] val from: Int
    protected[this] val until: Int
    override def foreach[C](f: A => C) {
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
    override def foreach[C](f: B => C) {
      for (x <- self)
        f(mapping(x))
    }
    override def stringPrefix = self.stringPrefix+"M"
  }

  trait FlatMapped[B] extends Transformed[B] {
    protected[this] val mapping: A => Traversable[B]
    override def foreach[C](f: B => C) {
      for (x <- self)
        for (y <- mapping(x))
          f(y)
    }
    override def stringPrefix = self.stringPrefix+"N"
  }

  trait Appended[B >: A] extends Transformed[B] {
    protected[this] val rest: Traversable[B]
    override def foreach[C](f: B => C) {
      for (x <- self) f(x)
      for (x <- rest) f(x)
    }
    override def stringPrefix = self.stringPrefix+"A"
  }

  trait Filtered extends Transformed[A] {
    protected[this] val pred: A => Boolean
    override def foreach[C](f: A => C) {
      for (x <- self)
        if (pred(x)) f(x)
    }
    override def stringPrefix = self.stringPrefix+"F"
  }

  trait TakenWhile extends Transformed[A] {
    protected[this] val pred: A => Boolean
    override def foreach[C](f: A => C) {
      for (x <- self) {
        if (!pred(x)) return
        f(x)
      }
    }
    override def stringPrefix = self.stringPrefix+"T"
  }

  trait DroppedWhile extends Transformed[A] {
    protected[this] val pred: A => Boolean
    override def foreach[C](f: A => C) {
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
  protected def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }

  override def ++[B >: A, That](that: Traversable[B])(implicit bf: BuilderFactory[B, That, This]): That = {
    newAppended(that).asInstanceOf[That]
// was:    val b = bf(repr)
//     if (b.isInstanceOf[NoBuilder[_]]) newAppended(that).asInstanceOf[That]
//    else super.++[B, That](that)(bf)
  }

  override def ++[B >: A, That](that: Iterator[B])(implicit bf: BuilderFactory[B, That, This]): That = ++[B, That](that.toStream)

  override def map[B, That](f: A => B)(implicit bf: BuilderFactory[B, That, This]): That = {
    newMapped(f).asInstanceOf[That]
// was:        val b = bf(repr)
//          if (b.isInstanceOf[NoBuilder[_]]) newMapped(f).asInstanceOf[That]
//    else super.map[B, That](f)(bf)
  }

  override def flatMap[B, That](f: A => Traversable[B])(implicit bf: BuilderFactory[B, That, This]): That = {
    newFlatMapped(f).asInstanceOf[That]
// was:    val b = bf(repr)
//     if (b.isInstanceOf[NoBuilder[_]]) newFlatMapped(f).asInstanceOf[That]
//    else super.flatMap[B, That](f)(bf)
  }

  override def filter(p: A => Boolean): This = newFiltered(p).asInstanceOf[This]
  override def init: This = newSliced(0, size - 1).asInstanceOf[This]
  override def drop(n: Int): This = newSliced(n max 0, MAX_INT).asInstanceOf[This]
  override def take(n: Int): This = newSliced(0, n).asInstanceOf[This]
  override def slice(from: Int, until: Int): This = newSliced(from max 0, until).asInstanceOf[This]
  override def dropWhile(p: A => Boolean): This = newDroppedWhile(p).asInstanceOf[This]
  override def takeWhile(p: A => Boolean): This = newTakenWhile(p).asInstanceOf[This]
  override def span(p: A => Boolean): (This, This) = (takeWhile(p), dropWhile(p))
  override def splitAt(n: Int): (This, This) = (take(n), drop(n))
}

