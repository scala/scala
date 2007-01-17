/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A mutable sequence that supports the remove operation and is ordered.
 *
 *  @author Sean McDirmid
 */
trait MutableSeq[A] extends MutableIterable[A] with Seq[A] {
  override def elements : SeqIterator[Int,A];

  override def isEmpty = super[MutableIterable].isEmpty;

  override def length = size;

  override def apply(idx : Int) = elements.seek(idx);

  def pfilter(p : A => Boolean) : MutableSeq[A] = new Filter(p);

  override def pmap[B](f : A => B) : MutableSeq[B] = new Map[B](f);

  /** Find the index of "a" in this sequence.
   *  @returns None if the "a" is not in this sequence.
   */
  def indexOf(a : A) = elements.indexOf(a);

  protected class Filter(p : A => Boolean) extends MutableSeq[A] {
    def elements : SeqIterator[Int,A] = new FilterIterator(MutableSeq.this.elements);
    class FilterIterator(underlying : SeqIterator[Int,A]) extends SeqIterator[Int,A] {
      private var index = 0;
      protected def seekNext : Option[A] = {
        while (underlying.hasNext) {
          val next = underlying.next;
          if (p(next)) return Some(next);
        }
        return None;
      }
      protected def seekPrevious : Option[A] = {
        while (underlying.hasPrevious) {
          val previous = underlying.previous;
          if (p(previous)) return Some(previous);
        }
        return None;
      }
      def hasNext : Boolean = seekNext match {
      case None => false;
      case Some(_) => underlying.previous; true;
      }
      def nextIndex = index;
      def next = seekNext match {
      case None => throw new NoSuchElementException;
      case Some(result) => index = index + 1; result;
      }
      def hasPrevious : Boolean = seekPrevious match {
      case None => false;
      case Some(_) => underlying.previous; true;
      }
      def previousIndex = {
        if (index == 0) throw new NoSuchElementException;
        index - 1;
      }
      def previous = seekPrevious match {
      case None => throw new NoSuchElementException;
      case Some(result) => index = index - 1; result;
      }
      def remove = underlying.remove;
    }
  }

  protected class Map[B](f : A => B) extends super.Map[B](f) with MutableSeq[B] {
    override def elements = MutableSeq.this.elements.map(f);
    override def apply(idx : Int) = f(MutableSeq.this.apply(idx));
  }
}
