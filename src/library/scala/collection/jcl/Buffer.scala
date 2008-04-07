/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A mutable sequence that supports element insertion and update.
 *
 *  @author Sean McDirmid
 */
trait Buffer[A] extends RandomAccessSeq.Mutable[A] with Ranged[Int,A] with MutableSeq[A] with Collection[A] {
  final protected type SortedSelf = Buffer[A];

  override def projection : Buffer.Projection[A] = new Buffer.Projection[A] {
    override def elements = Buffer.this.elements
    override def length = Buffer.this.length
    override def apply(idx : Int) = Buffer.this.apply(idx)
    override def transform(f : A => A) = Buffer.this.transform(f)
  }

  protected class DefaultBufferIterator extends DefaultSeqIterator with BufferIterator[Int,A] {
    override def set(a : A) = {
      if (index == 0) throw new NoSuchElementException
      Buffer.this.set(index - 1, a)
    }
    override def add(a : A) = {
      Buffer.this.add(index, a)
    }
  }
  override def elements : BufferIterator[Int,A] = new DefaultBufferIterator

  /** The first index of a buffer is 0. */
  override def firstKey = 0;

  /** The last index of a buffer is its size - 1. */
  override def lastKey = size - 1;

  /** Indices are compared through subtraction. */
  final def compare(k0 : Int, k1 : Int) = k0 - k1;

  /** Removes the element at index <code>idx</code> */
  def remove(idx : Int) = {
    val i = elements;
    val ret = i.seek(idx); i.remove; ret;
  }
  /** Removes N elements from index <code>idx</code> */
  def remove(idx : Int, length : Int) = {
    val i = elements
    i.seek(idx)
    for (j <- 0.until(length)) i.remove
  }
  /** replaces */
  def replace(from : Int, length : Int, added : Seq[A]) = {
    val min = if (length < added.length) length else added.length
    val i = added.elements
    var j = 0
    while (j < length && i.hasNext) {
      set(from + j, i.next); j = j + 1
    }
    assert(j == min)
    if (i.hasNext) {
      val slice = added.drop(length)
      assert(!slice.isEmpty)
      addAll(from + min, slice)
    } else if (j < length) {
      assert(length > min)
      remove(from + min, length - min)
    }
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
    for (that <- that) {
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
    override def length = {
      if (until != None) {
        if (from != None) until.get - from.get;
        else until.get;
      } else super.length;
    }
    override def elements : BufferIterator[Int,A] = new RangeIterator;
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
  /*
  protected class Map[B](f : A => B) extends super.Map[B](f) with Buffer.Projection[B] {
    override def elements = Buffer.this.elements.map[B](f);
    //override def apply(idx : Int) = f(MutableSeq.this.apply(idx));
    //override def size = length;
  }
  */
}
object Buffer {
  def apply[T](list : java.util.List[T]) = new BufferWrapper[T] {
    val underlying = list
  }

  trait Projection0[A] extends MutableSeq.Projection[A] with RandomAccessSeq.Projection[A] {
    override def projection : Projection0[A] = this
    override def elements : SeqIterator[Int,A] = new DefaultSeqIterator

    protected class MapProjection[B](f : A => B) extends super.MapProjection[B](f) with Projection0[B] {
      override def projection = this
    }
    override def map[B](f: A => B) : Projection0[B] = new MapProjection[B](f)
  }
  class Projection[A] extends Collection.Projection[A] with RandomAccessSeq.MutableProjection[A] with Projection0[A] with Buffer[A] {
    override def elements : BufferIterator[Int,A] = new DefaultBufferIterator
    override def projection : Buffer.Projection[A] = this
  }
}
