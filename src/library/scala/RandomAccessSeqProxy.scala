/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** This class implements a proxy for random access sequences. It forwards
 *  all calls to a different sequence object.
 *
 *  @author  Stepan Koltsov
 *  @version 1.0, 27/06/2008
 */
trait RandomAccessSeqProxy[+A] extends RandomAccessSeq[A] with SeqProxy[A] {
  override def self: RandomAccessSeq[A]

  override def projection = self.projection

  override def drop(from: Int): RandomAccessSeq[A] = self.drop(from)
  override def take(until: Int): RandomAccessSeq[A] = self.take(until)
  override def slice(from: Int, until: Int) : RandomAccessSeq[A] = self.slice(from, until)
  override def reverse = self.reverse
  override def partition(p: A => Boolean): (RandomAccessSeq[A], RandomAccessSeq[A]) =
    self.partition(p)
  override def patch[B >: A](from0: Int, patch0: RandomAccessSeq[B], replaced0: Int) =
    self.patch(from0, patch0, replaced0)
  override def ++[B >: A](that: Iterable[B]): RandomAccessSeq[B] = self ++ that
  override def toStream: Stream[A] = self.toStream
}

// vim: set ts=2 sw=2 et:
