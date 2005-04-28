/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


object Seq {
  def view[A <% Ordered[A]](xs: Seq[A]): Ordered[Seq[A]] = new Proxy(xs) with Ordered[Seq[A]] {
    def compareTo[B >: Seq[A] <% Ordered[B]](that: B): Int = that match {
      case ys: Seq[A] =>
        var res = 0;
        val xsit = xs.elements;
        val ysit = ys.elements;
        while (xsit.hasNext && ysit.hasNext && (res == 0)) {
          res = xsit.next compareTo ysit.next;
        }
        if (res != 0) res else if (xsit.hasNext) 1 else -1
      case _ =>
        -(that compareTo xs)
    }
  }
}


/** Class <code>Seq[A]</code> represents finite sequences of elements
 *  of type <code>A</code>.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Seq[+A] extends AnyRef with PartialFunction[Int, A] with Iterable[A] {

  /** Returns the length of the sequence.
  *
  *  @return the sequence length.
  */
  def length: Int;

  /** Is this partial function defined for the index <code>x</code>?
  *
  *  @return true, iff <code>x</code> is a legal sequence index.
  */
  def isDefinedAt(x: Int): Boolean = (x >= 0) && (x < length);

  /** Returns the index of the first occurence of the specified
  *  object in this sequence.
  *
  *  @param  elem  element to search for.
  *  @return the index in this sequence of the first occurence of the specified
  *  element, or -1 if the sequence does not contain this element.
  */
  def indexOf[B >: A](elem: B): Int = {
    val it = elements;
    var i = 0;
    var found = false;
    while (!found && it.hasNext) {
      if (it.next == elem) {
        found = true;
      } else {
        i = i + 1
      }
    }
    if (found) i else -1;
  }

  /** Returns the index of the last occurence of the specified
  *  element in this sequence, or -1 if the sequence does not
  *  contain this element.
  *
  *  @param  elem   element to search for.
  *  @return the index in this sequence of the last occurence of the
  *          specified  element, or -1 if the sequence does not contain
  *          this element.
  */
  def lastIndexOf[B >: A](elem: B): Int = {
    var i = length;
    var found = false;
    while (!found && (i > 0)) {
      i = i - 1;
      if (this(i) == elem) {
        found = true;
      }
    }
    if (found) i else -1;
  }

  /** Returns the sub-sequence starting from index <code>n</code>.
  */
  def take(n: Int): Seq[A] = subseq(0, n);

  /** Returns a new sub-sequence that drops the first <code>n</code>
  *  elements of this sequence.
  */
  def drop(n: Int): Seq[A] = subseq(n, length - n);

  /** Returns a subsequence starting from index <code>from</code>
  *  consisting of <code>len</code> elements.
  */
  def subseq(from: Int, len: Int): Seq[A] =
    if ((from + len) <= length) new Seq[A] {
      def apply(n: Int): A = Seq.this.apply(n + from);
      def length: Int = len;
      def elements: Iterator[A] = new Iterator[A] {
        var i = from;
        def hasNext = (i < (from + len));
        def next = {
          val res = Seq.this.apply(i);
          i = i + 1;
          res
        }
      }
    } else
      error("cannot create subsequence");

  /** Fills the given array <code>xs</code> with the elements of
  *  this sequence starting at position <code>start</code>.
  *
  *  @param  xs the array to fill.
  *  @param  start starting index.
  *  @return the given array <code>xs</code> filled with this list.
  */
  def copyToArray[B >: A](xs: Array[B], start: Int): Array[B] = {
    val it = elements;
    var i = start;
    while (it.hasNext) {
      xs(i) = it.next;
      i = i + 1;
    }
    xs
  }

  /** Transform this sequence into a list of all elements.
  *
  *  @return  a list which enumerates all elements of this sequence.
  */
  def toList: List[A] = elements.toList;

  /** Customizes the <code>toString</code> method.
  *
  *  @return a string representation of this sequence.
  */
  override def toString() = {
    val iter = elements;
    var res = stringPrefix + "(";
    if (iter.hasNext) {
      res = res + iter.next;
      while (iter.hasNext)
      res = res + ", " + iter.next;
    }
    res + ")"
  }

  /** Defines the prefix of the string representation.
  */
  protected def stringPrefix: String = "Seq";
}
