/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: IterableProxy.scala 15458 2008-06-28 20:23:22Z stepancheg $


package scala.collection.generic

import collection.mutable.Buffer

// Methods could be printed by  cat TraversibeTemplate.scala | egrep '^  (override )?def'


/** This trait implements a proxy for traversible objects. It forwards
 *  all calls to a different traversible object
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait TraversibleProxyTemplate[+A, +This <: TraversibleTemplate[A, This] with Traversible[A]] extends TraversibleTemplate[A, This] with Proxy {
  def self: This

  override def isEmpty: Boolean = self.isEmpty
  override def size: Int = self.size
  override def hasDefiniteSize = self.hasDefiniteSize
  override def ++[B >: A, That](that: Traversible[B])(implicit bf: BuilderFactory[B, That, This]): That = self.++(that)(bf)
  override def ++[B >: A, That](that: Iterator[B])(implicit bf: BuilderFactory[B, That, This]): That = self.++(that)(bf)
  override def map[B, That](f: A => B)(implicit bf: BuilderFactory[B, That, This]): That = self.map(f)(bf)
  override def flatMap[B, That](f: A => Traversible[B])(implicit bf: BuilderFactory[B, That, This]): That = self.flatMap(f)(bf)
  override def filter(p: A => Boolean): This = self.filter(p)
  override def remove(p: A => Boolean): This = self.remove(p)
  override def partition(p: A => Boolean): (This, This) = self.partition(p)
  override def groupBy[K](f: A => K): Map[K, This] = self.groupBy(f)
  override def foreach(f: A => Unit): Unit = self.foreach(f)
  override def forall(p: A => Boolean): Boolean = self.forall(p)
  override def exists(p: A => Boolean): Boolean = self.exists(p)
  override def count(p: A => Boolean): Int = self.count(p)
  override def find(p: A => Boolean): Option[A] = self.find(p)
  override def foldLeft[B](z: B)(op: (B, A) => B): B = self.foldLeft(z)(op)
  override def /: [B](z: B)(op: (B, A) => B): B = self./:(z)(op)
  override def foldRight[B](z: B)(op: (A, B) => B): B = self.foldRight(z)(op)
  override def :\ [B](z: B)(op: (A, B) => B): B = self.:\(z)(op)
  override def reduceLeft[B >: A](op: (B, A) => B): B = self.reduceLeft(op)
  override def reduceLeftOpt[B >: A](op: (B, A) => B): Option[B] = self.reduceLeftOpt(op)
  override def reduceRight[B >: A](op: (A, B) => B): B = self.reduceRight(op)
  override def reduceRightOpt[B >: A](op: (A, B) => B): Option[B] = self.reduceRightOpt(op)
  override def head: A = self.head
  override def headOption: Option[A] = self.headOption
  override def tail: This = self.tail
  override def last: A = self.last
  override def lastOption: Option[A] = self.lastOption
  override def init: This = self.init
  override def take(n: Int): This = self.take(n)
  override def drop(n: Int): This = self.drop(n)
  override def slice(from: Int, until: Int): This = self.slice(from, until)
  override def takeWhile(p: A => Boolean): This = self.takeWhile(p)
  override def dropWhile(p: A => Boolean): This = self.dropWhile(p)
  override def span(p: A => Boolean): (This, This) = self.span(p)
  override def splitAt(n: Int): (This, This) = self.splitAt(n)
  override def copyToBuffer[B >: A](dest: Buffer[B]) = self.copyToBuffer(dest)
  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) = self.copyToArray(xs, start, len)
  override def copyToArray[B >: A](xs: Array[B], start: Int) = self.copyToArray(xs, start)
  override def toArray[B >: A]: Array[B] = self.toArray
  override def toList: List[A] = self.toList
  override def toIterable: Iterable[A] = self.toIterable
  override def toSequence: Sequence[A] = self.toSequence
  override def toStream: Stream[A] = self.toStream
//  override def sortWith(lt : (A,A) => Boolean): This = self.sortWith(lt)
  override def mkString(start: String, sep: String, end: String): String = self.mkString(start, sep, end)
  override def mkString(sep: String): String = self.mkString(sep)
  override def mkString: String = self.mkString
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = self.addString(b, start, sep, end)
  override def addString(b: StringBuilder, sep: String): StringBuilder = self.addString(b, sep)
  override def addString(b: StringBuilder): StringBuilder = self.addString(b)
  override def toString = self.toString
  override def stringPrefix : String = self.stringPrefix
  override def view = self.view
  override def view(from: Int, until: Int): TraversibleView[A, This] = self.view(from, until)
}
