/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import Predef._

/** Buffered iterators are iterators which allow to inspect the next
 *  element without discarding it.
 *
 *  @author  Martin Odersky
 *  @author  SeanMcDirmid
 *  @version 2.0, 16/04/2007
 */
trait BufferedIterator[+A] extends Iterator[A] {
  /** Checks what the next available element is.
    *
    *  @return the current element
    */
  def head : A
  def headOpt = if (!hasNext) None else Some(head)
  override def buffered: this.type = this
  /** iterates over and applies <code>f</code> to the next element
   *  if this iterator has a next element that <code>f</code> is defined for.
   */
  def readIf[T](f : PartialFunction[A,T]) : Option[T] =
    if (hasNext && f.isDefinedAt(head))
      Some(f(next))
    else None

  /** iterates over elements as long as <code>f</code> is true
   *  for each element, returns whether anything was read
   */
  def readWhile(f : A => Boolean) : Boolean = {
    var read = false
    while (hasNext && f(head)) {
      next
      read = true
    }
    read
  }
  def advanced : BufferedIterator.Advanced[A] = new BufferedIterator.Default[A] {
    protected def fill(sz : Int) : Seq[A] = if (BufferedIterator.this.hasNext) (BufferedIterator.this.next) :: Nil else Nil
  }
}

object BufferedIterator {
  import collection.mutable.{Buffer, ListBuffer}

  trait Advanced[+A] extends BufferedIterator[A] {
    /** returns the first <code>sz</code> elements that will be iterated by this iterator,
     *  or fewer if the iterator has less elements left to iterate over
     */
    def peekList(sz : Int) : Seq[A]

    /** Checks what the next available element is.
     *
     *  @return the current element
     */
    def head: A = peek(0)

    /** return the <code>n</code>th element that will be iterated by this iterator */
    def peek(n : Int) : A = {
      val lst = peekList(n + 1);
      if (n == 0) {
        return if (lst.isEmpty) defaultPeek
               else lst(0)
      }

      val i = lst.elements
      var m = 0
      while (m < n && i.hasNext) {
        i.next; m += 1
      }
      if (!i.hasNext) defaultPeek
      else i.next
    }
    /** element returned when no element left to iterate over;
     *  throws <code>NoSuchElementException</code> by default
     */
    protected def defaultPeek : A = throw new Predef.NoSuchElementException

    /** true if elements of <code>seq</code> will be iterated over next in this iterator
     */
    def startsWith(seq : Seq[Any]) : Boolean = {
      var sz = seq.length
      val j = peekList(sz).elements
      val i = seq.elements
      while (i.hasNext && j.hasNext)
        if (i.next != j.next) return false
      return !i.hasNext
    }
    def readIfStartsWith(seq : Seq[Any]) : Boolean = {
      if (startsWith(seq)) {
        var i = 0
        while (i < seq.length) {
          next; i = i + 1
        }
        true
      } else false
    }
    override def counted : CountedIterator[A] with Advanced[A] = new CountedIterator[A] with Advanced[A] {
      private var cnt = -1
      def count = cnt
      override def hasNext: Boolean = Advanced.this.hasNext
      override def next: A = { cnt += 1; Advanced.this.next }
      override def peekList(sz : Int) : Seq[A] = Advanced.this.peekList(sz)
      override protected def defaultPeek : A = Advanced.this.defaultPeek
      override def counted : this.type = this
    }
    override def hasNext = !peekList(1).isEmpty
    override def toString = {
      val list = peekList(0)
      if (!list.isEmpty) "peek " + list else "***"
    }

    override def advanced : this.type = this
  }

  trait PutBack[+A] extends Advanced[A] {
    protected[this] def putBack(a : A) : Unit
    override def counted : CountedIterator[A] with PutBack[A] = new CountedIterator[A] with PutBack[A] {
      private var cnt = -1
      def count = cnt
      override protected def defaultPeek : A = PutBack.this.defaultPeek
      override def next: A = { cnt += 1; PutBack.this.next }
      override def hasNext: Boolean = PutBack.this.hasNext
      override def peekList(sz : Int) : Seq[A] = PutBack.this.peekList(sz)
      override protected[this] def putBack(a : A) : Unit = {
        if (cnt <= 0) throw new IllegalArgumentException
        PutBack.this.putBack(a)
        cnt = cnt - 1
      }
      override def counted : this.type = this
    }
    protected[this] def flushFrom[B <% Seq[A]](i : Default[B]) =
      i.forget.reverse.foreach(_.reverse.foreach(putBack))
  }


  abstract class Default[+A] extends PutBack[A] {
    import scala.collection.mutable.ListBuffer
    private[this] val lookahead = new ListBuffer[A]  // = Nil
    override protected[this] def putBack(a : A) : Unit = a +: lookahead
    override protected def defaultPeek : A = throw new Predef.NoSuchElementException
    override def hasNext = !lookahead.isEmpty || super.hasNext

    /** used to fill lookahead buffer. <code>sz</code> can be used by implementations as a heauristic to determine how many elements are desired */
    protected def fill(sz : Int) : Seq[A]

    private[BufferedIterator] def forget : List[A] = {
      val ret = lookahead.toList
      lookahead.clear
      ret
    }

    override def peekList(sz : Int) : Seq[A] = {
      if (sz == 0) return lookahead.readOnly
      else if (sz == 1) {
        if (!lookahead.isEmpty) return lookahead.readOnly
        fill(sz) match {
        case Seq.singleton(x) => lookahead += x
        case next => lookahead ++= next
        }
        return lookahead.readOnly
      }
      var sz0 = lookahead.length
      while (sz0 < sz) {
        val next = fill(sz - sz0)
        if (next.isEmpty) return lookahead.readOnly
        sz0 += next.length
        lookahead ++= next
      }
      lookahead.readOnly
    }
    override def next : A = {
      val lst = peekList(1)
      if (lst.isEmpty) throw new Predef.NoSuchElementException
      lookahead.remove(0)
      lst(0)
    }
  }
}


