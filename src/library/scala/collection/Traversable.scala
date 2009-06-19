/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Traversable.scala 15188 2008-05-24 15:01:02Z stepancheg $
package scala.collection

// import immutable.{List, Stream, Nil}
import mutable.{Buffer, ArrayBuffer, ListBuffer}
import util.control.Breaks
import generic._

/** A template trait for traversable collections.
 *
 *  Collection classes mixing in this trait provide a method
 *  <code>foreach</code> which traverses all the
 *  elements contained in the collection, applying a given procedure to each.
 *  They also provide a method `newBuilder`
 *  which creates a builder for collections of the same kind.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait Traversable[+A] extends TraversableTemplate[A, Traversable[A]]
                         with TraversableClass[A, Traversable] {
  def companion: Companion[Traversable] = Traversable

  /* The following methods are inherited from TraversableTemplate
   *
  override def isEmpty: Boolean
  override def size: Int
  override def hasDefiniteSize
  override def ++[B >: A, That](that: Traversable[B])(implicit bf: BuilderFactory[B, That, Traversable[A]]): That
  override def ++[B >: A, That](that: Iterator[B])(implicit bf: BuilderFactory[B, That, Traversable[A]]): That
  override def map[B, That](f: A => B)(implicit bf: BuilderFactory[B, That, Traversable[A]]): That
  override def flatMap[B, That](f: A => Traversable[B])(implicit bf: BuilderFactory[B, That, Traversable[A]]): That
  override def filter(p: A => Boolean): Traversable[A]
  override def remove(p: A => Boolean): Traversable[A]
  override def partition(p: A => Boolean): (Traversable[A], Traversable[A])
  override def groupBy[K](f: A => K): Map[K, Traversable[A]]
  override def foreach[U](f: A =>  U): Unit
  override def forall(p: A => Boolean): Boolean
  override def exists(p: A => Boolean): Boolean
  override def count(p: A => Boolean): Int
  override def find(p: A => Boolean): Option[A]
  override def foldLeft[B](z: B)(op: (B, A) => B): B
  override def /: [B](z: B)(op: (B, A) => B): B
  override def foldRight[B](z: B)(op: (A, B) => B): B
  override def :\ [B](z: B)(op: (A, B) => B): B
  override def reduceLeft[B >: A](op: (B, A) => B): B
  override def reduceLeftOpt[B >: A](op: (B, A) => B): Option[B]
  override def reduceRight[B >: A](op: (A, B) => B): B
  override def reduceRightOpt[B >: A](op: (A, B) => B): Option[B]
  override def head: A
  override def headOption: Option[A]
  override def tail: Traversable[A]
  override def last: A
  override def lastOption: Option[A]
  override def init: Traversable[A]
  override def take(n: Int): Traversable[A]
  override def drop(n: Int): Traversable[A]
  override def slice(from: Int, until: Int): Traversable[A]
  override def takeWhile(p: A => Boolean): Traversable[A]
  override def dropWhile(p: A => Boolean): Traversable[A]
  override def span(p: A => Boolean): (Traversable[A], Traversable[A])
  override def splitAt(n: Int): (Traversable[A], Traversable[A])
  override def copyToBuffer[B >: A](dest: Buffer[B])
  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int)
  override def copyToArray[B >: A](xs: Array[B], start: Int)
  override def toArray[B >: A]: Array[B]
  override def toList: List[A]
  override def toIterable: Iterable[A]
  override def toSequence: Sequence[A]
  override def toStream: Stream[A]
//  override def sortWith(lt : (A,A) => Boolean): Traversable[A]
  override def mkString(start: String, sep: String, end: String): String
  override def mkString(sep: String): String
  override def mkString: String
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder
  override def addString(b: StringBuilder, sep: String): StringBuilder
  override def addString(b: StringBuilder): StringBuilder
  override def toString
  override def stringPrefix : String
  override def view
  override def view(from: Int, until: Int): TraversableView[A, Traversable[A]]
  */
}

/** Factory methods and utilities for instances of type Traversable */
object Traversable extends TraversableFactory[Traversable] { self =>

  /** provide break functionality separate from client code */
  private[collection] val breaks: Breaks = new Breaks

  implicit def builderFactory[A]: BuilderFactory[A, Traversable[A], Coll] = new VirtualBuilderFactory[A]

  def newBuilder[A]: Builder[A, Traversable[A]] = immutable.Traversable.newBuilder[A]

  /** A wrapper class which adds `min` and `max` methods to iterables of an element type that has an Ordering.
   */
  class ComparableTraversableOps[A](self: Traversable[A], cmp: Ordering[A]) {

    /** Returns the minimal element of the wrapped iterable `self` with respect to the given ordering `cmp` */
    def min: A = {
      require(!self.isEmpty, "min(<empty>)")
      var acc = self.head
      for (x <- self)
        if (cmp.lt(x, acc)) acc = x
      acc
    }

    /** Returns the maximal element of the wrapped iterable `self` with respect to the given ordering `cmp` */
    def max: A = {
      require(!self.isEmpty, "max(<empty>)")
      var acc = self.head
      for (x <- self)
        if (cmp.gt(x, acc)) acc = x
      acc
    }
  }

  /** A wrapper class which adds `sum` and `product` methods to iterables of an element type that is `Numeric`.
   */
  class NumericTraversableOps[A](self: Traversable[A], num: Numeric[A]) {

    /** Returns the sum of all elements of the wrapped iterable `self` with respect to the numeric operations in `num` */
    def sum: A = {
      var acc = num.zero
      for (x <- self) acc = num.plus(acc, x)
      acc
    }

    /** Returns the product of all elements of the wrapped iterable `self` with respect to the numeric operations in `num` */
    def product: A = {
      var acc = num.one
      for (x <- self) acc = num.times(acc, x)
      acc
    }
  }

  /** A wrapper class which adds `flatten` and `transpose` methods to iterables or iterable element type`.
   */
  class TraversableTraversableOps[This <: Traversable[Traversable[A]], A](self: This) {

    /** Returns the concatenation of all elements of the wrapped iterable `self` */
    def flatten[That](implicit bf: BuilderFactory[A, That, This]): That = {
      val b = bf(self)
      for (xs <- self)
        b ++= xs
      b.result
    }

    /** Returns the transposition of the wrapped iterable `self`: rows become columns and columns become rows.
     */
    def transpose[Row, That](implicit bf: BuilderFactory[A, Row, This], bbf: BuilderFactory[Row, That, This]): That = {
      val bs: Array[Builder[A, Row]] = self.head.map(_ => bf(self))(Traversable.builderFactory[Builder[A, Row]]).toArray
      for (xs <- self) {
        var i = 0
        for (x <- xs) {
          bs(i) += x
          i += 1
        }
      }
      val bb = bbf(self)
      for (b <- bs) bb += b.result
      bb.result
    }
  }

  /** A wrapper class which adds an `unzip` method to iterable whose elements are pairs.
  	*/
  class PairTraversableOps[This <: Traversable[(A1, A2)], A1, A2](self: This) {

    /** Returns a pair of iterables consisting of the first, respectively second, component of all
     *  elements in the wrapped iterable `self`.
     */
    def unzip[That1, That2](implicit bf1: BuilderFactory[A1, That1, This], bf2: BuilderFactory[A2, That2, This]): (That1, That2) = {
      val b1 = bf1(self)
      val b2 = bf2(self)
      for ((x1, x2) <- self) {
        b1 += x1
        b2 += x2
      }
      (b1.result, b2.result)
    }
  }

  /** Implicit wrapper conversion of iterables with elements admitting comparison.
   *  @see ComparableTraversableOps
   */
  implicit def comparableTraversableWrapper[A](self: Traversable[A])(implicit cmp: Ordering[A]) =
    new ComparableTraversableOps(self, cmp)

  /** Implicit wrapper conversion of iterables with numeric elements.
   *  @see NumericTraversableOps
   */
  implicit def numericTraversableWrapper[A](self: Traversable[A])(implicit num: Numeric[A]) =
    new NumericTraversableOps(self, num)

  /** Implicit wrapper conversion of iterables with iterable elements.
   *  @see TraversableTraversableOps
   */
  implicit def traversableTraversableWrapper[This <: Traversable[Traversable[A]], A](self: This) =
    new TraversableTraversableOps[This, A](self) // !!! error if type parameters are omitted

  /** Implicit wrapper conversion of iterables with pairs as elements.
   *  @see PairTraversableOps
   */
  implicit def pairTraversableWrapper[This <: Traversable[(A1, A2)], A1, A2](self: This) =
    new PairTraversableOps[This, A1, A2](self)
}

