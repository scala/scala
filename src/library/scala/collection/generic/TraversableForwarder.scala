/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: IterableProxy.scala 15458 2008-06-28 20:23:22Z stepancheg $


package scala.collection.generic

import mutable.Buffer
// import immutable.{List, Nil, ::, Stream}

/** This trait implements a forwarder for traversable objects. It forwards
 *  all calls to a different iterable object, except for
 *
 *    - toString, hashCode, equals, stringPrefix
 *    - newBuilder, view
 *    - all calls creating a new iterable object of the same kind
 *
 *  The above methods are forwarded by subclass TraversableProxy
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait TraversableForwarder[+A] extends Traversable[A] {

  /** The iterable object to which calls are forwarded */
  protected def underlying: Traversable[A]

  // Iterable delegates
  // Iterable methods could be printed by  cat IterableTemplate.scala | sed -n '/trait Iterable/,$ p' | egrep '^  (override )?def'

  override def isEmpty = underlying.isEmpty
  override def hasDefiniteSize = underlying.hasDefiniteSize
  override def foreach(f: A => Unit) = underlying.foreach(f)
  override def forall(p: A => Boolean): Boolean = underlying.forall(p)
  override def exists(p: A => Boolean): Boolean = underlying.exists(p)
  override def count(p: A => Boolean): Int = underlying.count(p)
  override def find(p: A => Boolean): Option[A] = underlying.find(p)
  override def foldLeft[B](z: B)(op: (B, A) => B): B = underlying.foldLeft(z)(op)
  override def foldRight[B](z: B)(op: (A, B) => B): B = underlying.foldRight(z)(op)
  override def reduceLeft[B >: A](op: (B, A) => B): B = underlying.reduceLeft(op)
  override def reduceRight[B >: A](op: (A, B) => B): B = underlying.reduceRight(op)
  override def reduceLeftOpt[B >: A](op: (B, A) => B): Option[B] = underlying.reduceLeftOpt(op)
  override def reduceRightOpt[B >: A](op: (A, B) => B): Option[B] = underlying.reduceRightOpt(op)
  override def copyToBuffer[B >: A](dest: Buffer[B]) = underlying.copyToBuffer(dest)
  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) = underlying.copyToArray(xs, start, len)
  override def toArray[B >: A]: Array[B] = underlying.toArray
  override def toList: List[A] = underlying.toList
  override def toSequence: Sequence[A] = underlying.toSequence
  override def toStream: Stream[A] = underlying.toStream
  override def mkString(start: String, sep: String, end: String): String = underlying.mkString(start, sep, end)
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.addString(b, start, sep, end)

  override def head: A = underlying.head
  override def last: A = underlying.last
  override def lastOption: Option[A] = underlying.lastOption
}
