/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** Analogous to a Java sorted set.
 *
 *  @author Sean McDirmid
 */
trait SortedSet[A] extends scala.collection.SortedSet[A] with jcl.Set[A] with Sorted[A,A] {
  final protected type SortedSelf = SortedSet[A];
  override def keySet = this;
  def compare(a0 : A, a1 : A) : Int;
  override def first : A = {
    val i = elements;
    if (i.hasNext) i.next;
    else throw new NoSuchElementException;
  }
  override def subsetOf(that : scala.collection.Set[A]) = super[SortedSet].subsetOf(that);
  override def hasAll(that : Iterable[A]) = super[Sorted].hasAll(that.elements);

  override def last  : A = {
    var last : A = null.asInstanceOf[A];
    val i = elements;
    while (i.hasNext) last = i.next;
    if (last == null) throw new NoSuchElementException;
    else last;
  }
  override def rangeImpl(from : Option[A], until : Option[A]) : SortedSet[A] = new Range(from, until);
  override def pfilter(p : A => Boolean) : SortedSet[A] = new Filter(p);
  protected class Filter(p : A => Boolean) extends super.Filter(p) with SortedSet[A] {
    def compare(a0 : A, a1 : A) : Int = SortedSet.this.compare(a0, a1);
  }

  protected class Range(from : Option[A], until : Option[A]) extends Filter(key => {
    (from == None || (compare(from.get,key) <= 0)) &&
      (until == None || (compare(key,until.get) < 0));
  }) with SortedSet[A] {
    if (from == None && until == None) throw new IllegalArgumentException;
    if (from != None && until != None && !(SortedSet.this.compare(from.get, until.get) < 0))
      throw new IllegalArgumentException;
    override def elements : MutableIterator[A] =
      new RangeIterator(SortedSet.this.elements.buffered0);
    private def contains1(key : A) =
      (from == None || (compare(from.get,key) <= 0)) &&
        (until == None || (compare(key,until.get) < 0));
    override def has(elem : A) = contains1(elem) && SortedSet.this.has(elem);
    override def rangeImpl(from : Option[A], until : Option[A]) : SortedSet[A] = {
      if (this.from != None && from == None) return rangeImpl(this.from, until);
      if (this.until != None && until == None) return rangeImpl(from, this.until);
      if (from != None && compare(this.from.get, from.get) > 0) return rangeImpl(this.from, until);
      if (until != None && compare(this.until.get, until.get) < 0) return rangeImpl(from, this.until);
      SortedSet.this.rangeImpl(from, until);
    }
    class RangeIterator(underlying : MutableIterator[A]#Buffered) extends MutableIterator[A] {
      if (from != None)
        underlying.seekNext(a => compare(from.get, a) <= 0);

      private def okNext(a : A) =
        if (until == None) true;
        else compare(a, until.get) < 0;

      def hasNext = underlying.hasNext && okNext(underlying.peekNext);
      def next = underlying.seekNext(okNext) match {
      case Some(result) => underlying.next; result;
      case None => throw new NoSuchElementException;
      }
      def remove = underlying.remove;
    }
  }
}
