/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import scala.collection.mutable.Buffer
import scala.compat.StringBuilder


/** This class implements a proxy for iterable objects. It forwards
 *  all calls to a different iterable object.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */
trait IterableProxy[+A] extends Iterable[A] with Proxy {

  def self: Iterable[A]
  override def elements: Iterator[A] = self.elements
  @deprecated
  override def concat [B >: A](that: Iterable[B]): Iterable[B] = self concat that
  override def map[B](f: A => B): Iterable[B] = self map f
  override def flatMap[B](f: A => Iterable[B]): Iterable[B] = self flatMap f
  override def filter(p: A => Boolean): Iterable[A] = self filter p
  override def takeWhile(p: A => Boolean): Iterable[A] = self takeWhile p
  override def dropWhile(p: A => Boolean): Iterable[A] = self dropWhile p
  override def take(n: Int): Iterable[A] = self take n
  override def drop(n: Int): Iterable[A] = self drop n
  override def foreach(f: A => Unit): Unit = self foreach f
  override def forall(p: A => Boolean): Boolean = self forall p
  override def exists(p: A => Boolean): Boolean = self exists p
  override def find(p: A => Boolean): Option[A] = self find p
  override def findIndexOf(p: A => Boolean): Int = self findIndexOf p
  override def indexOf[B >: A](elem: B): Int = self indexOf elem
  override def foldLeft[B](z: B)(op: (B, A) => B): B = (self foldLeft z)(op)
  override def foldRight[B](z: B)(op: (A, B) => B): B = (self foldRight z)(op)
  override def /:[B](z: B)(op: (B, A) => B): B = (z /: self)(op)
  override def :\[B](z: B)(op: (A, B) => B): B = (self :\ z)(op)
  override def reduceLeft[B >: A](op: (B, B) => B): B = self reduceLeft op
  override def reduceRight[B >: A](op: (B, B) => B): B = self reduceRight op
  override def sameElements[B >: A](that: Iterable[B]): Boolean = self sameElements that
  override def copyToBuffer[B >: A](dest: Buffer[B]): Unit = self copyToBuffer dest
  override def toList: List[A] = self.toList
  override def mkString(start: String, sep: String, end: String): String = self.mkString(start, sep, end)
  override def addString(buf: StringBuilder, start: String, sep: String, end: String): StringBuilder = self.addString(buf, start, sep, end)
}
