package scala;

/* An abstract class representing an ordered collection of elements
* of type <code>a</code>.
* This class comes with two implementing case classes {link scala.Nil/} and
* {@link scala.::_class/} that implement the abstract members <code>isEmpty</code>,
* <code>head</code> and <code>tail</code>. But instead of this
*
* @arg a the type of the elements contained in the list.
*/
trait List[a] extends Seq[a] {

  /** Tests if this list is empty.
  * @return True iff the list contains no element.
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
  def ::(x: a): List[a] =
    new scala.::[a](x, this);

  /** Returns a list resulting from the concatenation of the given
  * list <code>prefix</code> and this list.
  * <p>
  * Ex:<br>
  * <code>[1, 2] ::: [3, 4] = [3, 4].:::([1, 2]) = [1, 2, 3, 4]</code>.
  * @param prefix the list to concatenate at the beginning of this list.
  * @return the concatenation of the two lists.
  */
  def :::(prefix: List[a]): List[a] =
    if (prefix.isEmpty) this
    else prefix.head :: (prefix.tail ::: this);

  /** Returns the number of elements in the list.
  * @return the number of elements in the list.
  */
  def length: Int = this match {
    case Nil => 0
    case _ :: xs => xs.length + 1
  }

  /** Returns the elements in the list as an iterator
   */
  def elements: Iterator[a] = new Iterator[a] {
    var current = List.this;
    def hasNext: Boolean = !current.isEmpty;
    def next: a = { val result = current.head; current = current.tail; result }
  }

  /** Returns the list without its last element.
   * @return the list without its last element.
   * @throws java.lang.RuntimeException if the list is empty.
   */
  def init: List[a] =
    if (isEmpty) error("Nil.init")
    else if (tail.isEmpty) Nil
    else head :: tail.init;

  /** Returns the last element of this list.
  * @return the last element of the list.
  * @throws java.lang.RuntimeException if the list is empty.
  */
  def last: a =
    if (isEmpty) error("Nil.last")
    else if (tail.isEmpty) head
    else tail.last;

  /** Returns the <code>n</code> first elements of this list.
  * @param n the number of elements to take.
  * @return the <code>n</code> first elements of this list.
  * @throws java.lang.RuntimeException if the list is too short.
  */
  def take(n: Int): List[a] =
    if (n == 0) Nil
    else head :: tail.take(n-1);

  /** Returns the list without its <code>n</code> first elements.
  * @param n the number of elements to drop.
  * @return the list without its <code>n</code> first elements.
  * @throws java.lang.RuntimeException if the list is too short.
  */
  def drop(n: Int): List[a] =
    if (n == 0) this
    else tail.drop(n-1);

  /** Returns the longest prefix of this list whose elements satisfy
  * the predicate <code>p</code>.
  * @param p the test predicate.
  * @return the longest prefix of this list whose elements satisfy
  * the predicate <code>p</code>.
  */
  def takeWhile(p: a => Boolean): List[a] =
    if (isEmpty || !p(head)) Nil
    else head :: tail.takeWhile(p);

  /** Returns the longest suffix of this list whose first element does not satisfy
  * the predicate <code>p</code>.
  * @param p the test predicate.
  * @return the longest suffix of the list whose first element does not satisfy
  * the predicate <code>p</code>.
  */
  def dropWhile(p: a => Boolean): List[a] =
    if (isEmpty || !p(head)) this
    else tail.dropWhile(p);

  /** Returns the <code>n</code>-th element of this list. The first element
   * (head of the list) is at position 0.
   * @param n index of the element to return
   * @return the element at position <code>n</code> in this list.
   * @throws java.lang.RuntimeException if the list is too short.
   */
  def apply(n: Int) = drop(n).head;

  def at(n: Int) = drop(n).head;

  /** Returns the list resulting from applying the given function <code>f</code> to each
  * element of this list.
  * @param f function to apply to each element.
  * @return <code>[f(a0), ..., f(an)]</code> if this list is <code>[a0, ..., an]</code>.
  */
  def map[b](f: a => b): List[b] =
    if (isEmpty) Nil
    else f(head) :: tail.map(f);

  /** Apply the given function <code>f</code> to each element of this list (while respecting
  * the order of the elements).
  * @param f the treatment to apply to each element.
  */
  def foreach(f: a => Unit): Unit =
    if (isEmpty) {} else { f(head); tail.foreach(f) };

  /** Returns all the elements of this list that satisfy the
   * predicate <code>p</code>. The order of the elements is preserved.
   * @param p the redicate used to filter the list.
   * @return the elements of this list satisfying <code>p</code>.
   */
  def filter(p: a => Boolean): List[a] =
    if (isEmpty) this
    else if (p(head)) head :: tail.filter(p)
    else tail.filter(p);

  /** Tests if the predicate <code>p</code> is satisfied by all elements in this
  * list.
  * @param p the test predicate.
  * @return True iff all elements of this list satisfy the predicate <code>p</code>.
  */
  def forall(p: a => Boolean): Boolean = isEmpty || (p(head) &&
  tail.forall(p));

  /** Tests the existence in this list of an element that satisfies the predicate
  * <code>p</code>.
  * @param p the test predicate.
  * @return True iff there exists an element in this list that satisfies
  * the predicate <code>p</code>.
  */
  def exists(p: a => Boolean): Boolean =
    !isEmpty && (p(head) || tail.exists(p));

  /** Combines the elements of this list together using the binary
  * operator <code>op</code>, from left to right, and starting with
  * the value <code>z</code>. Similar to <code>fold</code> but with
  * a different order of the arguments, allowing to use nice constructions like
  * <code>(z foldl_: l) { ... }</code>.
  * @return <code>op(... (op(op(z,a0),a1) ...), an)</code> if the list
  * is <code>[a0, a1, ..., an]</code>.
  */
  def foldl_:[b](z: b)(f: (b, a) => b): b = match {
    case Nil => z
    case x :: xs => (xs.foldl_:[b](f(z, x)))(f)
  }

  def foldr[b](z: b)(f: (a, b) => b): b = match {
    case Nil => z
    case x :: xs => f(x, (xs foldr z)(f))
  }

  def foldl1(f: (a, a) => a): a = this match {
    case Nil => error("foldl1 of empty list")
    case x :: xs => (x foldl_: xs)(f)
  }

  def foldr1(f: (a, a) => a): a = match {
    case Nil => error("foldr1 of empty list")
    case x :: Nil => x
    case x :: xs => f(x, xs foldr1 f)
  }

  /** Applies the given function <code>f</code> to each element of this list, then concatenates
  * the results.
  * @param f the function to apply on each element.
  * @return <code>f(a0) ::: ... ::: f(an)</code> if this list is
  * <code>[a0, ..., an]</code>.
  */
  def flatMap[b](f: a => List[b]): List[b] =
    if (isEmpty) Nil
    else f(head) ::: tail.flatMap(f);

  /** Reverses the elements of this list.
  * <p>
  * Ex: <br>
  * <code>[1, 2, 3] reverse = [3, 2, 1]</code>.
  * @return the elements of this list in reverse order.
  */
  def reverse: List[a] = {
    ((Nil: List[a]) foldl_: this)((xs: List[a], x: a) => x :: xs)
  }

  /** Prints on standard output a raw textual representation of this list.
  * <p>
  * Ex: <br>
  * <code>[1, 2, 3] print</code> will display <code>1 :: 2 :: 3 :: []</code>.
  */
  def print: Unit =
    if (isEmpty) java.lang.System.out.println("Nil")
    else {
      java.lang.System.out.print(head as java.lang.Object);
      java.lang.System.out.print(" :: ");
      tail.print
    }
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
  def copyToArray(xs: Array[a], start: Int): Array[a] = match {
    case Nil => xs
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
  def mkString(start: String, sep: String, end: String): String =
    start +
  (if (isEmpty) end
   else if (tail.isEmpty) head.toString() + end
   else head.toString().concat(sep).concat(tail.mkString("", sep, end)));

  override def toString() = mkString("[", ",", "]");

  /** Return a list formed from this list and the specified list
  * <code>that</code> by associating each element of the former with
  * the element at the same position in the latter.
  * @param that must have the same length as the self list.
  * @return <code>[(a0,b0), ..., (an,bn)]</code> when
  * <code>[a0, ..., an] zip [b0, ..., bn]</code> is invoked.
  * @throws java.lang.RuntimeException if lists have different lengths.
  */
  def zip[b](that: List[b]): List[Tuple2[a,b]] =
    if (this.isEmpty || that.isEmpty) Nil
    else Tuple2(this.head, that.head) :: this.tail.zip(that.tail);

  /** Tests if the given value <code>elem</code> is a member of
  * this list.
  * @param elem element whose membership has to be tested.
  * @return True iff there is an element of this list which is
  * equal (w.r.t. <code>==</code>) to <code>elem</code>.
  */
  def contains(elem: a) = exists(
     new Object with Function1[a, Boolean] {
       def apply(x: a): Boolean = x == elem;
     });

  /** Computes the union of this list and the given list
  * <code>that</code>.
  * @param that the list of elements to add to the list.
  * @return a list without doubles containing the elements of this
  * list and those of the given list <code>that</code>.
  */
  def union(that: List[a]): List[a] =
    if (this.isEmpty) that
    else {
      val result = this.tail union that;
      if (that contains this.head) result else this.head :: result;
    }

  /** Computes the difference between this list and the given list
  * <code>that</code>.
  * @param that the list of elements to remove from this list.
  * @return this list without the elements of the given list <code>that</code>.
  */
  def diff(that: List[a]): List[a] =
    if (this.isEmpty || that.isEmpty) this
    else {
      val result = this.tail diff that;
      if (that contains this.head) result else this.head :: result;
    }

  /** Computes the intersection between this list and the given list
  * <code>that</code>.
  * @param that the list to intersect.
  * @return the list of elements contained both in this list and
  * in the given list <code>that</code>.
  */
  def intersect(that: List[a]): List[a] = filter(x => that contains x);

  /** Removes redundant elements from the list. Uses the method <code>==</code>
  * to decide if two elements are identical.
  * @return the list without doubles.
  */
  def removeDuplicates: List[a] =
    if (isEmpty) this
    else {
      val rest = tail.removeDuplicates;
      if (rest contains head) rest else head :: rest
    }
}

object List {

  def range(from: Int, end: Int): List[Int] =
    if (from >= end) scala.Predef.List()
    else from :: range(from + 1, end);

}


