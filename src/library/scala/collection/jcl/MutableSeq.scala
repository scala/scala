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
trait MutableSeq[A] extends Seq[A] with MutableIterable[A]  {
  protected class DefaultSeqIterator extends SeqIterator[Int,A] {
    protected var index = 0
    override def hasNext = index < length
    override def next = {
      if (!hasNext) throw new NoSuchElementException("no lookahead")
      index = index + 1
      MutableSeq.this.apply(index - 1)
    }
    override def hasPrevious = index > 0
    override def previous = {
      if (!hasPrevious) throw new NoSuchElementException
      index = index - 1
      MutableSeq.this.apply(index)
    }

    override def nextIndex = index
    override def previousIndex = {
      if (index == 0) throw new NoSuchElementException
      else index - 1
    }
    def remove = throw new UnsupportedOperationException
  }
  override def elements : SeqIterator[Int,A] = new DefaultSeqIterator;

  override def isEmpty = super[MutableIterable].isEmpty;

  override def apply(idx : Int) = elements.seek(idx);
  override def projection : MutableSeq.Projection[A] = new MutableSeq.Projection[A] {
    override def length = MutableSeq.this.length
    override def elements = MutableSeq.this.elements
    override def apply(idx : Int) = MutableSeq.this.apply(idx)
  }

  /** Find the index of "a" in this sequence.
   *  @returns None if the "a" is not in this sequence.
   */
  def indexOf(a : A) = elements.indexOf(a);

  override def length = {
    var i = elements;
    var sz = 0;
    while (i.hasNext) {
      sz = sz + 1;
      i.next;
    }
    sz;
  }
  protected trait Filter extends MutableSeq.Projection[A] {
    protected def p(a : A) : Boolean
    override def elements : SeqIterator[Int,A] = new FilterIterator(MutableSeq.this.elements);
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
  protected class Map[B](f : A => B) extends super.Map[B](f) with MutableSeq.Projection[B] {
    override def elements = MutableSeq.this.elements.map(f);
    override def apply(idx : Int) = f(MutableSeq.this.apply(idx));
    override def size = length;
  }
}
object MutableSeq {
  trait Projection[A] extends MutableSeq[A] with MutableIterable.Projection[A] with Seq.Projection[A] {
    override def projection = this
    override def filter(pp : A => Boolean) : Projection[A] = new Filter {
      override def p(a : A) = pp(a)
    }
    override def map[B](f : A => B) : Projection[B] = new Map[B](f);
  }
}

