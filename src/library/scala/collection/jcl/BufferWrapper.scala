/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** Wraps Java lists.
 *
 *  @author Sean McDirmid
 */
trait BufferWrapper[A] extends Buffer[A] with CollectionWrapper[A] {
  def underlying : java.util.List[A];
  override def elements : BufferIterator[Int,A] = new IteratorWrapper(underlying.listIterator);
  override def remove(idx : Int) = underlying.remove(idx).asInstanceOf[A];
  override def add(a : A) = underlying.add(a);
  override def add(idx : Int, a : A) = underlying.add(idx,a);
  override def addAll(idx : Int, that : Iterable[A]) = that match {
  case that : CollectionWrapper[_] => underlying.addAll(idx, that.underlying); {}
  case _ => super.addAll(idx, that);
  }
  override def indexOf(a : A) = {
    val result = underlying.indexOf(a);
    if (result == -1) None;
    else Some(result);
  }
  override def apply(idx : Int) = underlying.get(idx).asInstanceOf[A];
  override def set(idx : Int, a : A) = underlying.set(idx, a).asInstanceOf[A];
  override def rangeImpl(from : Option[Int], until : Option[Int]) : Buffer[A] = new Range(from, until);
  protected class Range(from : Option[Int], until : Option[Int]) extends super.Range(from,until) with BufferWrapper[A] {
    val underlying = {
      val fromi = if (from == None) 0 else from.get;
      val toi = if (until == None) BufferWrapper.this.size else until.get;
      BufferWrapper.this.underlying.subList(fromi, toi);
    }
    override def elements = super[BufferWrapper].elements;
  }
  class IteratorWrapper(underlying : java.util.ListIterator[A]) extends MutableIterator.Wrapper[A](underlying) with BufferIterator[Int,A] {
    def add(a : A) = underlying.add(a);
    def set(a : A) = underlying.set(a);
    def hasPrevious = underlying.hasPrevious;
    def previous = underlying.previous.asInstanceOf[A];
    def previousIndex = underlying.previousIndex;
    def nextIndex = underlying.nextIndex;
  }
  override def length = underlying.size;
}
