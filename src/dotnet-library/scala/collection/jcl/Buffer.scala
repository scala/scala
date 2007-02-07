/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A mutable sequence that supports element insertion and update.
 *
 *  @author Sean McDirmid
 */
trait Buffer[A] extends MutableSeq[A] with Collection[A] with Ranged[Int,A] {
  final protected type SortedSelf = Buffer[A];

  override def elements : BufferIterator[Int,A];
  /** The first index of a buffer is 0. */
  override def first = 0;
  /** The last index of a buffer is its size - 1. */
  override def last = size - 1;
  /** Indices are compared through subtraction. */
  final def compare(k0 : Int, k1 : Int) = k0 - k1;
  /** Removes the element at index "idx" */
  def remove(idx : Int) = {
    val i = elements;
    val ret = i.seek(idx); i.remove; ret;
  }

  /** Replaces the element at index "idx" with "a."
    * @returns the element replaced.
    */
  def set(idx : Int, a : A) : A = {
    val i = elements;
    val ret = i.seek(idx); i.set(a); ret;
  }

  /** Equivalent to set except the replaced element is not returned. */
  def update(idx : Int, a : A) : Unit = set(idx, a);

  /** @returns always true. */
  def add(a : A) : Boolean = {
    val i = elements;
    while (i.hasNext) i.next;
    i.add(a);
    true;
  }

  /** Inserts "a" into this buffer just before the element at index "idx." */
  def add(idx: Int, a: A): Unit = {
    val i = elements; i.seek(idx);
    i.add(a);
  }

  /** Inserts all elements of <code>that</code> into this buffer just before
   *  the element at index <code>idx</code>.
   *
   *  @param idx  ..
   *  @param that ..
   */
  def addAll(idx: Int, that: Iterable[A]): Unit = {
    val i = elements; i.seek(idx);
    for (val that <- that) {
      i.add(that); i.next;
    }
  }

  override def transform(f: A => A): Boolean = {
    var changed = false;
    val i = elements;
    while (i.hasNext) {
      val a0 = i.next;
      val a1 = f(a0);
      if (a0 != a1) {
        i.set(a1); changed = true;
      }
    }
    changed;
  }
  override def +(a : A) : this.type = super[Collection].+(a);
  override def -=(a : A) = super[Collection].-=(a);
  override def isEmpty = super[MutableSeq].isEmpty;
  override def pfilter(p : A => Boolean) : MutableSeq[A] = super[MutableSeq].pfilter(p);
  override def rangeImpl(from : Option[Int], until : Option[Int]) : Buffer[A] = new Range(from, until);

  protected class Range(var from : Option[Int], var until : Option[Int]) extends Buffer[A] {
    if (from == None && until == None) throw new IllegalArgumentException;
    if (from != None && until != None && !(from.get < until.get)) throw new IllegalArgumentException;
    override def add(a : A) =
      if (until == None) Buffer.this.add(a);
      else {
        Buffer.this.add(until.get, a);
        true;
      }
    private def translate(idx : Int) = {
      if (until != None && idx > until.get) throw new IllegalArgumentException;
      else if (from != None) from.get + idx;
      else idx;
    }
    override def apply(idx : Int) : A = Buffer.this.apply(translate(idx));
    override def set(idx : Int, a : A) = Buffer.this.set(translate(idx), a);
    override def add(idx : Int, a : A) = Buffer.this.add(translate(idx), a);
    override def remove(idx : Int) = Buffer.this.remove(translate(idx));
    override def size = {
      if (until != None) {
        if (from != None) until.get - from.get;
        else until.get;
      } else super.size;
    }
    def elements : BufferIterator[Int,A] = new RangeIterator;
    class RangeIterator extends BufferIterator[Int,A] {
      val underlying = Buffer.this.elements;
      if (from != None) underlying.seek(from.get);
      def hasNext = underlying.hasNext &&
        (until == None || underlying.nextIndex < until.get);
      def hasPrevious = underlying.hasPrevious &&
        (from == None || underlying.previousIndex >= from.get);
      def next = {
        if (until != None && underlying.nextIndex >= until.get) throw new NoSuchElementException;
        underlying.next;
      }
      def previous = {
        if (from != None && underlying.previousIndex < from.get) throw new NoSuchElementException;
        underlying.previous;
      }
      def add(a : A) = {
        if (until != None && underlying.nextIndex > until.get) throw new NoSuchElementException;
        if (from != None && underlying.previousIndex < from.get) throw new NoSuchElementException;
        underlying.add(a);
        if (until != None) until = Some(until.get + 1);
      }
      def set(a : A) = {
        if (until != None && underlying.nextIndex > until.get) throw new NoSuchElementException;
        if (from != None && underlying.previousIndex < from.get) throw new NoSuchElementException;
        underlying.set(a);
      }
      def remove = {
        if (until != None && underlying.nextIndex > until.get) throw new NoSuchElementException;
        if (from != None && underlying.previousIndex < from.get) throw new NoSuchElementException;
        underlying.remove;
      }
      def nextIndex = {
        val ret = underlying.nextIndex;
        if (until != None && ret >= until.get) throw new NoSuchElementException;
        if (from != None) ret - from.get;
        else ret;
      }
      def previousIndex = {
        val ret = underlying.previousIndex;
        if (from != None && ret < from.get) throw new NoSuchElementException;
        if (from != None) ret - from.get;
        else ret;
      }
    }
  }
}
