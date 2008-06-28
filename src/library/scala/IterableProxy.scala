/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import scala.collection.mutable.Buffer


/** This class implements a proxy for iterable objects. It forwards
 *  all calls to a different iterable object.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */
trait IterableProxy[+A] extends Iterable[A] with Proxy {

  override def self: Iterable[A]

  override def elements = self.elements
  @deprecated
  override def concat [B >: A](that: Iterable[B]) = self concat that
  override def ++[B >: A](that: Iterable[B]) = self ++ that
  override def map[B](f: A => B) = self map f
  override def flatMap[B](f: A => Iterable[B]) = self flatMap f
  override def filter(p: A => Boolean) = self filter p
  override def partition(p: A => Boolean) = self partition p
  override def takeWhile(p: A => Boolean) = self takeWhile p
  override def dropWhile(p: A => Boolean) = self dropWhile p
  @deprecated override def take(n: Int) = self take n
  @deprecated override def drop(n: Int) = self drop n
  override def foreach(f: A => Unit) = self foreach f
  override def forall(p: A => Boolean) = self forall p
  override def exists(p: A => Boolean) = self exists p
  override def find(p: A => Boolean) = self find p
  @deprecated override def findIndexOf(p: A => Boolean) = self findIndexOf p
  @deprecated override def indexOf[B >: A](elem: B): Int = self indexOf elem
  override def foldLeft[B](z: B)(op: (B, A) => B) = (self foldLeft z)(op)
  override def foldRight[B](z: B)(op: (A, B) => B) = (self foldRight z)(op)
  override def /:[B](z: B)(op: (B, A) => B) = (z /: self)(op)
  override def :\[B](z: B)(op: (A, B) => B) = (self :\ z)(op)
  override def reduceLeft[B >: A](op: (B, A) => B) = self reduceLeft op
  override def reduceRight[B >: A](op: (A, B) => B) = self reduceRight op
  override def copyToBuffer[B >: A](dest: Buffer[B]) = self copyToBuffer dest
  override def sameElements[B >: A](that: Iterable[B]) = self sameElements that

  override def toList = self.toList
  override def toSeq = self.toSeq
  override def toStream = self.toStream

  override def mkString(start: String, sep: String, end: String) = self.mkString(start, sep, end)
  override def mkString(sep: String) = self.mkString(sep)
  override def mkString = self.mkString

  override def addString(buf: StringBuilder, start: String, sep: String, end: String) =
    self.addString(buf, start, sep, end)

  override def addString(buf: StringBuilder, sep: String) =
    self.addString(buf, sep)

  override def addString(buf: StringBuilder) =
    self.addString(buf)

  override def copyToArray[B >: A](xs: Array[B], start: Int) = self.copyToArray(xs, start)

  override def isEmpty = self.isEmpty

  override def projection = self.projection

  override def hasDefiniteSize = self.hasDefiniteSize
}
