/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


/** This class implements a proxy for sequences. It forwards
 *  all calls to a different sequence object.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 2.0, 31/12/2006
 */
trait SeqProxy[+A] extends Seq[A] with CollectionProxy[A] {

  override def self: Seq[A]

  // PartialFunction delegates

  override def apply(i: Int): A = self(i)

  // Seq delegates
  // Seq methods could be printed by  cat Seq.scala | sed -n '/trait Seq/,$ p' | egrep '^  (override )?def'

  override def length: Int = self.length
  override def lengthCompare(l: Int) = self lengthCompare l
  override def size = self.size
  override def isEmpty = self.isEmpty
  @deprecated
  override def concat [B >: A](that: Iterable[B]): Seq[B] = self concat that
  override def last = self.last
  override def lastOption = self.lastOption
  override def first = self.first
  override def firstOption = self.firstOption
  @deprecated
  override def headOption = firstOption

  override def ++ [B >: A](that: Iterable[B]) = self ++ that
  override def isDefinedAt(x: Int): Boolean = self isDefinedAt x

  override def lastIndexOf[B >: A](elem: B): Int = self lastIndexOf elem
  override def findIndexOf(p: A => Boolean): Int = self findIndexOf p

  override def indexOf[B >: A](elem: B): Int = self indexOf elem

  override def map[B](f: A => B): Seq[B] = self map f
  override def flatMap[B](f: A => Iterable[B]) = self flatMap f
  override def filter(p: A => Boolean): Seq[A] = self filter p
  override def take(n: Int): Seq[A] = self take n
  override def drop(n: Int): Seq[A] = self drop n

  override def slice(from: Int, len: Int): Seq[A] = self.slice(from, len)
  @deprecated
  override def slice(from: Int) = slice(from, length)

  override def takeWhile(p: A => Boolean): Seq[A] = self takeWhile p
  override def dropWhile(p: A => Boolean): Seq[A] = self dropWhile p

  override def reverse: Seq[A] = self.reverse

  override def contains(elem: Any): Boolean = self contains elem
  @deprecated
  override def subseq(from: Int, end: Int) = slice(from, end - from)

  override def toArray[B >: A]: Array[B] = self.toArray

  override def toSeq = self.toSeq

  override def projection = self.projection

  override def equalsWith[B](that: Seq[B])(f: (A, B) => Boolean): Boolean = self.equalsWith(that)(f)

  override def startsWith[B](that: Seq[B]) = self startsWith that
  override def endsWith[B](that: Seq[B]) = self endsWith that

  override def indexOf[B >: A](that: Seq[B]) = self indexOf that

  override def containsSlice[B](that: Seq[B]) = self containsSlice that
}
