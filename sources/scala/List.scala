/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/**
 * This object provides methods for creating specialized lists, and for
 * transforming special kinds of lists (e.g. lists of lists).
 *
 *  @author  Martin Odersky and others
 *  @version 1.0, 15/07/2003
 */
object List {
  /**
   * Create a sorted list of all integers in a range.
   * @return the sorted list of all integers in range [from;end).
   */
  def range(from: Int, end: Int): List[Int] =
    if (from >= end) Nil
    else from :: range(from + 1, end);

  /**
   * Create a list containing several copies of an element.
   * @param n the length of the resulting list
   * @param elem the element composing the resulting list
   * @return a list composed of n elements all equal to elem
   */
  def make[a](n: int, elem: a): List[a] =
    if (n == 0) Nil
    else elem :: make(n - 1, elem);

  /**
   * Create a list by applying a function to successive integers.
   * @param n the length of the resulting list
   * @param maker the procedure which, given an integer n, returns the
   * nth element of the resulting list, where n is in [0;n).
   * @return the list obtained by applying the maker function to successive
   * integers from 0 to n (exclusive).
   */
  def tabulate[a](n: int, maker: int => a): List[a] = {
    def loop(i: int): List[a] =
      if (i == n) Nil
      else maker(i) :: loop(i+1);
    loop(0)
  }

  /**
   * Concatenate all the elements of a given list of lists.
   * @param l the list of lists that are to be concatenated
   * @return the concatenation of all the lists
   */
  def flatten[a](l: List[List[a]]): List[a] = l match {
    case Nil => Nil
    case head :: tail => head ::: flatten(tail)
  }

  /**
   * Transform a list of pair into a pair of lists.
   * @param l the list of pairs to unzip
   * @return a pair of lists: the first list in the pair contains the
   * list
   */
  def unzip[a,b](l: List[Pair[a,b]]): Pair[List[a], List[b]] = l match {
    case Nil => Pair(Nil, Nil)
    case Pair(f, s) :: tail =>
      val Pair(fs, ss) = unzip(tail);
      Pair(f :: fs, s :: ss)
  }
}

/** A trait representing an ordered collection of elements of type
 *  <code>a</code>. This class comes with two implementing case
 *  classes <code>scala.Nil</code> and <code>scala.::</code> that
 *  implement the abstract members <code>isEmpty</code>,
 *  <code>head</code> and <code>tail</code>.
 *
 *  @author  Martin Odersky and others
 *  @version 1.0, 16/07/2003
 */
trait List[+a] extends Seq[a]/* with StructuralEquality[List[a]]*/ {

  /** Tests if this list is empty.
   *  @return true, iff the list contains no element.
   */
  def isEmpty: Boolean;

  /** Returns this first element of the list.
  * @return the first element of this list.
  * @throws java.lang.RuntimeException if the list is empty.
  */
  def head: a;

  /** Returns this list without its first element.
  * @return this list without its first element.
  * @throws java.lang.RuntimeException if the list is empty.
  */
  def tail: List[a];

  /** Add an element <code>x</code> at the beginning of this list.
  * <p>
  * Ex:<br>
  * <code>1 :: [2, 3] = [2, 3].::(1) = [1, 2, 3]</code>.
  * @param x the element to append.
  * @return the list with <code>x</code> appended at the beginning.
  */
  def ::[b >: a](x: b): List[b] =
    new scala.::(x, this);

  /** Returns a list resulting from the concatenation of the given
  * list <code>prefix</code> and this list.
  * <p>
  * Ex:<br>
  * <code>[1, 2] ::: [3, 4] = [3, 4].:::([1, 2]) = [1, 2, 3, 4]</code>.
  * @param prefix the list to concatenate at the beginning of this list.
  * @return the concatenation of the two lists.
  */
  def :::[b >: a](prefix: List[b]): List[b] = prefix match {
    case Nil => this
    case head :: tail => head :: (tail ::: this);
  };

  /** Reverse the given prefix and append the current list to that.
   * This function is equivalent to an application of <code>reverse</code>
   * on the prefix followed by a call to <code>:::</code>, but more
   * efficient (and tail recursive).
   * @param prefix the prefix to reverse and then prepend
   * @return the concatenation of the reversed prefix and the current list.
   */
  def reverse_:::[b >: a](prefix: List[b]): List[b] = prefix match {
    case Nil => this
    case head :: tail => tail reverse_::: (head :: this)
  };

  /** Returns the number of elements in the list.
  * @return the number of elements in the list.
  */
  def length: Int = match {
    case Nil => 0
    case _ :: xs => xs.length + 1
  };

  /** Returns the elements in the list as an iterator
   */
  def elements: Iterator[a] = new Iterator[a] {
    var current = List.this;
    def hasNext: Boolean = !current.isEmpty;
    def next: a =
      if (!hasNext)
	error("next on empty Iterator")
      else {
        val result = current.head; current = current.tail; result
      };
  };

  /** Returns the list without its last element.
   * @return the list without its last element.
   * @throws java.lang.RuntimeException if the list is empty.
   */
  def init: List[a] = match {
    case Nil => error("Nil.init")
    case _ :: Nil => Nil
    case head :: tail => head :: tail.init
  };

  /** Returns the last element of this list.
  * @return the last element of the list.
  * @throws java.lang.RuntimeException if the list is empty.
  */
  def last: a = match {
    case Nil => error("Nil.last")
    case last :: Nil => last
    case _ :: tail => tail.last
  }

  /** Returns the <code>n</code> first elements of this list.
  * @param n the number of elements to take.
  * @return the <code>n</code> first elements of this list.
  * @throws java.lang.RuntimeException if the list is too short.
  */
  def take(n: Int): List[a] =
    if (n == 0) Nil
    else head :: (tail take (n-1));

  /** Returns the list without its <code>n</code> first elements.
  * @param n the number of elements to drop.
  * @return the list without its <code>n</code> first elements.
  * @throws java.lang.RuntimeException if the list is too short.
  */
  def drop(n: Int): List[a] =
    if (n == 0) this
    else (tail drop (n-1));

  /** Return the rightmost <code>n</code> elements from this list.
   * @param n the number of elements to take
   * @return the suffix of length <code>n</code> of the list
   * @throws java.lang.RuntimeException if the list is too short.
   */
  def takeRight(n: Int): List[a] = {
    def loop(lead: List[a], lag: List[a]): List[a] = lead match {
      case Nil => lag
      case _ :: tail => loop(tail, lag.tail)
    };
    loop(drop(n), this)
  };

  /** Return the list wihout its rightmost <code>n</code> elements.
   * @param n the number of elements to take
   * @return the suffix of length <code>n</code> of the list
   * @throws java.lang.RuntimeException if the list is too short.
   */
  def dropRight(n: Int): List[a] = {
    def loop(lead: List[a], lag: List[a]): List[a] = lead match {
      case Nil => Nil
      case _ :: tail => lag.head :: loop(tail, lag.tail)
    }
    loop(drop(n), this)
  }

  /** Split the list at a given point and return the two parts thus
   * created.
   * @param n the position at which to split
   * @return a pair of lists composed of the first <code>n</code>
   * elements, and the other elements.
   */
  def splitAt(n: Int): Pair[List[a], List[a]] =
    if (n == 0) Pair(Nil, this)
    else {
      val Pair(tail1, tail2) = tail splitAt (n-1);
      Pair(head :: tail1, tail2)
    };

  /** Returns the longest prefix of this list whose elements satisfy
  * the predicate <code>p</code>.
  * @param p the test predicate.
  * @return the longest prefix of this list whose elements satisfy
  * the predicate <code>p</code>.
  */
  def takeWhile(p: a => Boolean): List[a] =
    if (isEmpty || !p(head)) Nil
    else head :: (tail takeWhile p);

  /** Returns the longest suffix of this list whose first element does not satisfy
  * the predicate <code>p</code>.
  * @param p the test predicate.
  * @return the longest suffix of the list whose first element does not satisfy
  * the predicate <code>p</code>.
  */
  def dropWhile(p: a => Boolean): List[a] =
    if (isEmpty || !p(head)) this
    else tail dropWhile p;

  /** Return the longest prefix of the list whose elements all satisfy
   * the given predicate, and the rest of the list.
   * @param p the test predicate
   * @return a pair consisting of the longest prefix of the list whose
   * elements all satisfy <code>p</code>, and the rest of the list.
   */
  def span(p: a => Boolean): Pair[List[a], List[a]] = match {
    case Nil => Pair(Nil, Nil)
    case head :: tail =>
      if (p(head)) {
        val Pair(tail1, tail2) = tail span p;
        Pair(head :: tail1, tail2)
      } else
        Pair(Nil, this)
  };

  /** Like <code>span</code> but with the predicate inverted.
   */
  def break(p: a => Boolean): Pair[List[a], List[a]] = span { x => !p(x) };

  /** Returns the <code>n</code>-th element of this list. The first element
   * (head of the list) is at position 0.
   * @param n index of the element to return
   * @return the element at position <code>n</code> in this list.
   * @throws java.lang.RuntimeException if the list is too short.
   */
  def apply(n: Int): a = drop(n).head;

  /** Returns the list resulting from applying the given function <code>f</code> to each
  * element of this list.
  * @param f function to apply to each element.
  * @return <code>[f(a0), ..., f(an)]</code> if this list is <code>[a0, ..., an]</code>.
  */
  def map[b](f: a => b): List[b] = match {
    case Nil => Nil
    case head :: tail => f(head) :: (tail map f)
  };

  /**
   * Apply a function to all the elements of the list, and return the
   * reversed list of results. This is equivalent to a call to <code>map</code>
   * followed by a call to <code>reverse</code>, but more efficient.
   * @param f the function to apply to each elements.
   * @return the reversed list of results.
   */
  def reverseMap[b](f: a => b): List[b] = {
    def loop(l: List[a], res: List[b]): List[b] = l match {
      case Nil => res
      case head :: tail => loop(tail, f(head) :: res)
    }
    loop(this, Nil)
  };

  /** Apply the given function <code>f</code> to each element of this list
  * (while respecting the order of the elements).
  * @param f the treatment to apply to each element.
  */
  def foreach(f: a => Unit): Unit = match {
    case Nil => ()
    case head :: tail => f(head); tail foreach f
  };

  /** Returns all the elements of this list that satisfy the
   * predicate <code>p</code>. The order of the elements is preserved.
   * @param p the redicate used to filter the list.
   * @return the elements of this list satisfying <code>p</code>.
   */
  def filter(p: a => Boolean): List[a] = match {
    case Nil => this
    case head :: tail =>
      if (p(head)) head :: (tail filter p) else tail filter p
  };

  /**
   * Remove all elements of the list which satisfy the predicate
   * <code>p</code>. This is like <code>filter</code> with the
   * predicate inversed.
   * @param p the predicate to use to test elements
   * @return the list without all elements which satisfy <code>p</code>
   */
  def remove(p: a => Boolean): List[a] = match {
    case Nil => this
    case head :: tail =>
      if (p(head)) tail remove p else head :: (tail remove p)
  };

  /**
   * Partition the list in two sub-lists according to a predicate.
   * @param p the predicate on which to partition
   * @return a pair of lists: the list of all elements which satisfy
   * <code>p</code> and the list of all elements which do not. The
   * relative order of the elements in the sub-lists is the same as in
   * the original list. */

  def partition(p: a => Boolean): Pair[List[a], List[a]] = match {
    case Nil => Pair(Nil, Nil)
    case head :: tail =>
      val Pair(taily, tailn) = tail partition p;
      if (p(head)) Pair(head :: taily, tailn)
      else Pair(taily, head :: tailn)
  };

  /**
   * Count the number of elements in the list which satisfy a predicate.
   * @param p the predicate for which to count
   * @return the number of elements satisfying <code>p</code>.
   */
  def count(p: a => Boolean): Int = match {
    case Nil => 0
    case head :: tail => if (p(head)) 1 + (tail count p) else (tail count p)
  };

  /** Tests if the predicate <code>p</code> is satisfied by all elements in this
  * list.
  * @param p the test predicate.
  * @return True iff all elements of this list satisfy the predicate <code>p</code>.
  */
  def forall(p: a => Boolean): Boolean =
    isEmpty || (p(head) && (tail forall p));

  /** Tests the existence in this list of an element that satisfies the predicate
  * <code>p</code>.
  * @param p the test predicate.
  * @return True iff there exists an element in this list that satisfies
  * the predicate <code>p</code>.
  */
  def exists(p: a => Boolean): Boolean =
    !isEmpty && (p(head) || (tail exists p));

  /**
   * Find and return the first element of the list satisfying a
   * predicate, if any.
   * @param p the predicate
   * @return the first element in the list satisfying <code>p</code>,
   * or <code>None</code> if none exists.
   */
  def find(p: a => Boolean): Option[a] = match {
    case Nil => None
    case head :: tail => if (p(head)) Some(head) else tail find p
  };

  /** Combines the elements of this list together using the binary
  * operator <code>op</code>, from left to right, and starting with
  * the value <code>z</code>. Similar to <code>fold</code> but with
  * a different order of the arguments, allowing to use nice constructions like
  * <code>(z foldLeft l) { ... }</code>.
  * @return <code>op(... (op(op(z,a0),a1) ...), an)</code> if the list
  * is <code>[a0, a1, ..., an]</code>.
  */
  def foldLeft[b](z: b)(f: (b, a) => b): b = match {
    case Nil => z
    case x :: xs => xs.foldLeft[b](f(z, x))(f)
  };

  def foldRight[b](z: b)(f: (a, b) => b): b = match {
    case Nil => z
    case x :: xs => f(x, xs.foldRight(z)(f))
  };

  def /:[b](z: b)(f: (b, a) => b): b = foldLeft(z)(f);
  def :/[b](z: b)(f: (a, b) => b): b = foldRight(z)(f);

  def reduceLeft[b >: a](f: (b, b) => b): b = this match {
    case Nil => error("Nil.reduceLeft")
    case x :: xs => ((xs: List[b]) foldLeft (x: b))(f)
  };

  def reduceRight[b >: a](f: (b, b) => b): b = match {
    case Nil => error("Nil.reduceRight")
    case x :: Nil => x: b
    case x :: xs => f(x, xs reduceRight f)
  };

  /**
  * Applies the given function <code>f</code> to each element of
  * this list, then concatenates the results.
  * @param f the function to apply on each element.
  * @return <code>f(a0) ::: ... ::: f(an)</code> if this list is
  * <code>[a0, ..., an]</code>.
  */
  def flatMap[b](f: a => List[b]): List[b] = match {
    case Nil => Nil
    case head :: tail => f(head) ::: (tail flatMap f)
  };

  /** Reverses the elements of this list.
  * <p>
  * Ex: <br>
  * <code>[1, 2, 3] reverse = [3, 2, 1]</code>.
  * @return the elements of this list in reverse order.
  */
  def reverse: List[a] =
    foldLeft(Nil : List[a])((xs, x) => x :: xs);

  /*
  def toArray: Array[a] = {
    val xs = new Array[a](length);
  copyToArray(xs, 0);
  xs
  }
  */

  /** Fills the given array <code>xs</code> with the elements of
  * this list starting at position <code>start</code>. Does not
  * work with empty lists.
  * @param xs the array to fill.
  * @param start starting index.
  * @return the given array <code>xs</code> filled with this list.
  * @throws error if the list is empty.
  */
  def copyToArray[b >: a](xs: Array[b], start: Int): int = match {
    case Nil => start
    case y :: ys => xs(start) = y; ys.copyToArray(xs, start + 1)
  }

  /** Returns a string representation of this list. The resulting string
  * begins with the string <code>start</code> and is finished by the string
  * <code>end</code>. Inside, the string representations of elements (w.r.t.
  * the method <code>toString()</code>) are separated by the string
  * <code>sep</code>.
  * <p>
  * Ex: <br>
  * <code>List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"</code>
  * @param start starting string.
  * @param sep separator string.
  * @param end ending string.
  * @return a string representation of this list.
  */
  def mkString(start: String, sep: String, end: String): String = match {
    case Nil => start + end
    case last :: Nil => start + last + end
    case fst :: tail => start + fst + sep + tail.mkString("", sep, end)
  }

  override def toString() = mkString("List(", ",", ")");

  /** Return a list formed from this list and the specified list
  * <code>that</code> by associating each element of the former with
  * the element at the same position in the latter.
  * @param that must have the same length as the self list.
  * @return <code>[(a0,b0), ..., (an,bn)]</code> when
  * <code>[a0, ..., an] zip [b0, ..., bn]</code> is invoked.
  * @throws java.lang.RuntimeException if lists have different lengths.
  */
  def zip[b](that: List[b]): List[Pair[a,b]] =
    if (this.isEmpty || that.isEmpty) Nil
    else Pair(this.head, that.head) :: this.tail.zip(that.tail);

  /** Tests if the given value <code>elem</code> is a member of
  * this list.
  * @param elem element whose membership has to be tested.
  * @return True iff there is an element of this list which is
  * equal (w.r.t. <code>==</code>) to <code>elem</code>.
  */
  def contains(elem: Any): boolean = exists { x => x == elem };

  /** Computes the union of this list and the given list
  * <code>that</code>.
  * @param that the list of elements to add to the list.
  * @return a list without doubles containing the elements of this
  * list and those of the given list <code>that</code>.
  */
  def union[b >: a](that: List[b]): List[b] = match {
    case Nil => that
    case head :: tail =>
      if (that contains head) tail union that
      else head :: (tail union that)
  }

  /** Computes the difference between this list and the given list
  * <code>that</code>.
  * @param that the list of elements to remove from this list.
  * @return this list without the elements of the given list <code>that</code>.
  */
  def diff[b >: a](that: List[b]): List[b] = match {
    case Nil => Nil
    case head :: tail =>
      if (that contains head) tail diff that
      else head :: (tail diff that)
  }

  /** Computes the intersection between this list and the given list
  * <code>that</code>.
  * @param that the list to intersect.
  * @return the list of elements contained both in this list and
  * in the given list <code>that</code>.
  */
  def intersect[b >: a](that: List[b]): List[b] = filter(x => that contains x);

  /** Removes redundant elements from the list. Uses the method <code>==</code>
  * to decide if two elements are identical.
  * @return the list without doubles.
  */
  def removeDuplicates: List[a] = match {
    case Nil => this
    case head :: tail =>
      if (tail contains head) tail.removeDuplicates
      else head :: tail.removeDuplicates
  }
}

