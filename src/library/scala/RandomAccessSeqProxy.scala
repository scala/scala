/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
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

  override def drop(from: Int): RandomAccessSeq[A] = self.drop(from)
  override def take(until: Int): RandomAccessSeq[A] = self.take(until)
  override def slice(from: Int, until: Int) : RandomAccessSeq[A] = self.slice(from, until)
  override def partition(p: A => Boolean): (RandomAccessSeq[A], RandomAccessSeq[A]) =
    self.partition(p)
  // XXX: def patch, reverse, should not return projection
  override def ++[B >: A](that: Iterable[B]): RandomAccessSeq[B] = self ++ that
  override def toStream: Stream[A] = self.toStream
}

// vim: set ts=2 sw=2 et:
