/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** The <code>Iterator</code> object provides various functions for
 *  creating specialized iterators.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.1, 04/02/2004
 */
object Iterator {

  def empty[a] = new Iterator[a] {
    def hasNext = false;
    def next: a = error("next on empty iterator");
  }

  def fromValues[a](xs: a*) = xs.elements;

  def fromArray[a](xs: Array[a]) = new Iterator[a] {
    private var i = 0;
    def hasNext: Boolean = i < xs.length;
    def next: a =
      if (i < xs.length) { val x = xs(i) ; i = i + 1 ; x }
      else error("next on empty iterator");
  }

  def fromString(str: String): Iterator[Char] = new Iterator[Char] {
    private var i = 0;
    private val len = str.length();
    def hasNext = i < len;
    def next = { val c = str charAt i; i = i + 1; c };
  }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + 1</code>
   *  where <code>e<sub>0</sub> = lo</code>
   *  and <code>e<sub>i</sub> &lt; end</code>.
   *
   *  @param lo the start value of the iterator
   *  @param end the end value of the iterator
   *  @return the iterator starting at value <code>lo</code>.
   */
  def range(lo: Int, end: Int): Iterator[Int] =
    range(lo, end, 1);

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + step</code>
   *  where <code>e<sub>0</sub> = lo</code>
   *  and <code>e<sub>i</sub> &lt; end</code>.
   *
   *  @param lo the start value of the iterator
   *  @param end the end value of the iterator
   *  @param step the increment value of the iterator
   *  @return the iterator starting at value <code>lo</code>.
   */
  def range(lo: Int, end: Int, step: Int) = new Iterator[Int] {
    private var i = lo;
    def hasNext: Boolean =  i < end;
    def next: Int =
      if (i < end) { val j = i; i = i + step; j } else error("next on empty iterator");
    def head: Int =
      if (i < end) i else error("head on empty iterator");
  }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = step(e<sub>n</sub>)</code>
   *  where <code>e<sub>0</sub> = lo</code>
   *  and <code>e<sub>i</sub> &lt; end</code>.
   *
   *  @param lo the start value of the iterator
   *  @param end the end value of the iterator
   *  @param step the increment function of the iterator
   *  @return the iterator starting at value <code>lo</code>.
   */
  def range(lo: Int, end: Int, step: Int => Int) = new Iterator[Int] {
    private var i = lo;
    def hasNext: Boolean =  i < end;
    def next: Int =
      if (i < end) { val j = i; i = step(i); j } else error("next on empty iterator");
    def head: Int =
      if (i < end) i else error("head on empty iterator");
  }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + 1</code>
   *  where <code>e<sub>0</sub> = lo</code>.
   *
   *  @param lo the start value of the iterator
   *  @return the iterator starting at value <code>lo</code>.
   */
  def from(lo: Int): Iterator[Int] =
    from(lo, 1);

  /** Create an iterator with elements
   * <code>e<sub>n+1</sub> = e<sub>n</sub> + step</code>
   *  where <code>e<sub>0</sub> = lo</code>.
   *
   *  @param lo the start value of the iterator
   *  @param step the increment value of the iterator
   *  @return the iterator starting at value <code>lo</code>.
   */
  def from(lo: Int, step: Int) = new Iterator[Int] {
    private var i = 0;
    def hasNext: Boolean = true;
    def next: Int = { val j = i; i = i + step; j }
  }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = step(e<sub>n</sub>)</code>
   *  where <code>e<sub>0</sub> = lo</code>.
   *
   *  @param lo the start value of the iterator
   *  @param step the increment function of the iterator
   *  @return the iterator starting at value <code>lo</code>.
   */
  def from(lo: Int, step: Int => Int) = new Iterator[Int] {
    private var i = 0;
    def hasNext: Boolean = true;
    def next: Int = { val j = i; i = step(i); j }
  }

}

/** Iterators are data structures that allow to iterate over a sequence
 *  of elements. They have a <code>hasNext</code> method for checking
 *  if there is a next element available, and a <code>next</code> method
 *  which returns the next element and discards it from the iterator.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.2, 15/03/2004
 */
trait Iterator[+a] with Iterable[a] {

  def hasNext: Boolean;

  def next: a;

  def elements: Iterator[a] = this;

  def take(n: Int) = new Iterator[a] {
    var remaining = n;
    def hasNext = remaining > 0 && Iterator.this.hasNext;
    def next: a =
      if (hasNext) { remaining = remaining - 1; Iterator.this.next }
      else error("next on empty iterator");
  }

  def drop(n: Int): Iterator[a] =
  	if (n > 0) { next; drop(n - 1) } else this;

  def map[b](f: a => b): Iterator[b] = new Iterator[b] {
    def hasNext = Iterator.this.hasNext;
    def next = f(Iterator.this.next)
  }

  def append[b >: a](that: Iterator[b]) = new Iterator[b] {
    def hasNext = Iterator.this.hasNext || that.hasNext;
    def next = if (Iterator.this.hasNext) Iterator.this.next else that.next;
  }

  def flatMap[b](f: a => Iterator[b]): Iterator[b] = new Iterator[b] {
    private var cur: Iterator[b] = Iterator.empty;
    def hasNext: Boolean =
      if (cur.hasNext) true
      else if (Iterator.this.hasNext) {
        cur = f(Iterator.this.next);
        hasNext
      } else false;
    def next: b =
      if (cur.hasNext) cur.next
      else if (Iterator.this.hasNext) {
        cur = f(Iterator.this.next);
        next
      } else error("next on empty iterator");
  }

  def filter(p: a => Boolean): Iterator[a] = new BufferedIterator[a] {
    private val source =
      Iterator.this.buffered;
    private def skip: Unit =
      while (source.hasNext && !p(source.head)) { source.next; () }
    def hasNext: Boolean = { skip; source.hasNext }
    def next: a = { skip; source.next }
    def head: a = { skip; source.head; }
  }

  def zip[b](that: Iterator[b]) = new Iterator[Pair[a, b]] {
    def hasNext = Iterator.this.hasNext && that.hasNext;
    def next = Pair(Iterator.this.next, that.next);
  }

  def buffered: BufferedIterator[a] = new BufferedIterator[a] {
    private var hd: a = _;
    private var ahead: Boolean = false;
    def head: a = {
      if (!ahead) {
        hd = Iterator.this.next;
        ahead = true
      }
      hd
    }
    def next: a =
      if (ahead) { ahead = false; hd } else head;
    def hasNext: Boolean = ahead || Iterator.this.hasNext;
    override def buffered: BufferedIterator[a] = this;
  }

  def duplicate: Pair[Iterator[a], Iterator[a]] = {
    var xs: List[a] = Nil;
    var ahead: Iterator[a] = null;
    class Partner extends Iterator[a] {
      var ys: List[a] = Nil;
      def hasNext: Boolean = Iterator.this.synchronized {
        ((this == ahead) && Iterator.this.hasNext) ||
      	((this != ahead) && (!xs.isEmpty || !ys.isEmpty || Iterator.this.hasNext));
      }
      def next: a = Iterator.this.synchronized {
		if (this == ahead) {
		  val e = Iterator.this.next;
		  xs = e :: xs; e
		} else {
		  if (ys.isEmpty) {
			ys = xs.reverse;
			xs = Nil;
		  }
		  ys match {
			case Nil =>
			  val e = Iterator.this.next;
			  ahead = this;
			  xs = e :: xs; e
			case z :: zs =>
			  ys = zs; z
		  }
		}
      }
    }
    ahead = new Partner;
    Pair(ahead, new Partner)
  }

  /** converts a prefix of this iterator to a sequence
  def toSeq( len:int ):Seq[a] = new Seq[a] {
    def length: Int = len;
    def elements = Iterator.this;
    def apply( i:int ) = {
      val it = Iterator.this;
      var j = 0; while( i<i ) it.next;
      it.next
    }
  }

  def toSeq:Seq[a] = toList;
  */

}
