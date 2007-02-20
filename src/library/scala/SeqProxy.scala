/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
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
trait SeqProxy[+A] extends Seq[A] with IterableProxy[A] {

  def self: Seq[A]

  override def apply(i: Int): A = self(i)
  override def length: Int = self.length
  override def isEmpty: Boolean = self.isEmpty
  @deprecated
  override def concat [B >: A](that: Iterable[B]): Seq[B] = self concat that
  override def isDefinedAt(x: Int): Boolean = self isDefinedAt x
  override def lastIndexOf[B >: A](elem: B): Int = self lastIndexOf elem
  override def map[B](f: A => B): Seq[B] = self map f
  override def flatMap[B](f: A => Iterable[B]): Seq[B] = self flatMap f
  override def filter(p: A => Boolean): Seq[A] = self filter p
  override def take(n: Int): Seq[A] = self take n
  override def drop(n: Int): Seq[A] = self drop n
  override def takeWhile(p: A => Boolean): Seq[A] = self takeWhile p
  override def dropWhile(p: A => Boolean): Seq[A] = self dropWhile p
  override def reverse: Seq[A] = self.reverse
  override def contains(elem: Any): Boolean = self contains elem
  override def slice(from: Int, len: Int): Seq[A] = self.slice(from, len)
  override def toArray[B >: A]: Array[B] = self.toArray
  override def copyToArray[B >: A](xs: Array[B], start: Int): Unit = self.copyToArray(xs, start)
}
