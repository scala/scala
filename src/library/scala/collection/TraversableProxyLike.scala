/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection

import generic._
import mutable.{Buffer, StringBuilder}
import scala.reflect.ClassTag

// Methods could be printed by  cat TraversableLike.scala | egrep '^  (override )?def'

/** This trait implements a proxy for Traversable objects. It forwards
 *  all calls to a different Traversable object.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait TraversableProxyLike[+A, +Repr <: TraversableLike[A, Repr] with Traversable[A]] extends TraversableLike[A, Repr] with Proxy {
  def self: Repr

  override def foreach[U](f: A => U): Unit = self.foreach(f)
  override def isEmpty: Boolean = self.isEmpty
  override def nonEmpty: Boolean = self.nonEmpty
  override def size: Int = self.size
  override def hasDefiniteSize = self.hasDefiniteSize
  override def ++[B >: A, That](xs: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = self.++(xs)(bf)
  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = self.map(f)(bf)
  override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = self.flatMap(f)(bf)
  override def filter(p: A => Boolean): Repr = self.filter(p)
  override def filterNot(p: A => Boolean): Repr = self.filterNot(p)
  override def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That = self.collect(pf)(bf)
  override def partition(p: A => Boolean): (Repr, Repr) = self.partition(p)
  override def groupBy[K](f: A => K): immutable.Map[K, Repr] = self.groupBy(f)
  override def forall(p: A => Boolean): Boolean = self.forall(p)
  override def exists(p: A => Boolean): Boolean = self.exists(p)
  override def count(p: A => Boolean): Int = self.count(p)
  override def find(p: A => Boolean): Option[A] = self.find(p)
  override def foldLeft[B](z: B)(op: (B, A) => B): B = self.foldLeft(z)(op)
  override def /: [B](z: B)(op: (B, A) => B): B = self./:(z)(op)
  override def foldRight[B](z: B)(op: (A, B) => B): B = self.foldRight(z)(op)
  override def :\ [B](z: B)(op: (A, B) => B): B = self.:\(z)(op)
  override def reduceLeft[B >: A](op: (B, A) => B): B = self.reduceLeft(op)
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = self.reduceLeftOption(op)
  override def reduceRight[B >: A](op: (A, B) => B): B = self.reduceRight(op)
  override def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = self.reduceRightOption(op)
  override def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = self.scanLeft(z)(op)(bf)
  override def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = self.scanRight(z)(op)(bf)
  override def sum[B >: A](implicit num: Numeric[B]): B = self.sum(num)
  override def product[B >: A](implicit num: Numeric[B]): B = self.product(num)
  override def min[B >: A](implicit cmp: Ordering[B]): A = self.min(cmp)
  override def max[B >: A](implicit cmp: Ordering[B]): A = self.max(cmp)
  override def head: A = self.head
  override def headOption: Option[A] = self.headOption
  override def tail: Repr = self.tail
  override def last: A = self.last
  override def lastOption: Option[A] = self.lastOption
  override def init: Repr = self.init
  override def take(n: Int): Repr = self.take(n)
  override def drop(n: Int): Repr = self.drop(n)
  override def slice(from: Int, until: Int): Repr = self.slice(from, until)
  override def takeWhile(p: A => Boolean): Repr = self.takeWhile(p)
  override def dropWhile(p: A => Boolean): Repr = self.dropWhile(p)
  override def span(p: A => Boolean): (Repr, Repr) = self.span(p)
  override def splitAt(n: Int): (Repr, Repr) = self.splitAt(n)
  override def copyToBuffer[B >: A](dest: Buffer[B]) = self.copyToBuffer(dest)
  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) = self.copyToArray(xs, start, len)
  override def copyToArray[B >: A](xs: Array[B], start: Int) = self.copyToArray(xs, start)
  override def copyToArray[B >: A](xs: Array[B]) = self.copyToArray(xs)
  override def toArray[B >: A: ClassTag]: Array[B] = self.toArray
  override def toList: List[A] = self.toList
  override def toIterable: Iterable[A] = self.toIterable
  override def toSeq: Seq[A] = self.toSeq
  override def toIndexedSeq: immutable.IndexedSeq[A] = self.toIndexedSeq
  override def toBuffer[B >: A] = self.toBuffer
  override def toStream: Stream[A] = self.toStream
  override def toSet[B >: A]: immutable.Set[B] = self.toSet
  override def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = self.toMap(ev)
  override def toTraversable: Traversable[A] = self.toTraversable
  override def toIterator: Iterator[A] = self.toIterator
  override def mkString(start: String, sep: String, end: String): String = self.mkString(start, sep, end)
  override def mkString(sep: String): String = self.mkString(sep)
  override def mkString: String = self.mkString
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = self.addString(b, start, sep, end)
  override def addString(b: StringBuilder, sep: String): StringBuilder = self.addString(b, sep)
  override def addString(b: StringBuilder): StringBuilder = self.addString(b)
  override def stringPrefix : String = self.stringPrefix
  override def view = self.view
  override def view(from: Int, until: Int): TraversableView[A, Repr] = self.view(from, until)
  // This appears difficult to override due to the type of WithFilter.
  // override def withFilter(p: A => Boolean): WithFilter = self.withFilter(p)
}
