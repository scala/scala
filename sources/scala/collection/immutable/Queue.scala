/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;

/** <code>Queue</code> objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  @author  Erik Stenman
 *  @version 1.0, 08/07/2003
 */

object Queue {
    val Empty:Queue[All] = new Queue(Nil, Nil);
}

class Queue[+A](in:List[A],out:List[A]) extends Seq[A] {

    def at(n: Int): A =
	if (n < out.length) out.at(n)
	    else (in.reverse).at(n - out.length);

  /** Returns the <code>n</code>-th element of this queue.
   * The first element is at position 0.
   * @param n index of the element to return
   * @return the element at position <code>n</code> in this list.
   * @throws java.lang.RuntimeException if the list is too short.
   */
    def apply(n: Int): A = at(n);

    /** Returns the elements in the list as an iterator
     */
    def elements: Iterator[A] = (out:::(in.reverse)).elements;

    /** Checks if the queue is empty.
     *
     *  @returns true, iff there is no element in the queue.
     */
    def isEmpty: Boolean = (in.isEmpty && out.isEmpty);

    /** Returns the lenegth of the queue.
     */
    def length = in.length + out.length;

    /** Creates a new queue with element added at the end
     *  of the old queue.
     *
     *  @param  elem        the element to insert
     */
    def +[B >: A](elem: B):Queue[B] = {
	new Queue(elem::in,out);
    }

    /** Returns a new que with all all elements provided by
     *  an <code>Iterable</code> object added at the end of
     *  the queue.
     *  The elements are prepended in the order they
     *  are given out by the iterator.
     *
     *  @param  iter        an iterable object
     */
    def +[B >: A](iter: Iterable[B]) = {
	var q:List[B] = in;
	iter.elements.foreach(e => q = (e::q));
	new Queue(q,out);
    }

    /** Returns a new queue with all elements added.
     *
     *  @param  elems		the elements to add.
     */
    def enqueue [B >: A](elems: B*): Queue[B] = (this + elems);

    /** Returns a tuple with the first element in the queue,
     *  and a new queu with this element removed.
     *
     *  @returns the first element of the queue.
     */
    def dequeue: Pair[A,Queue[A]] = {
	var newOut:List[A]=Nil;
	var newIn:List[A]=Nil;
        if (out.isEmpty) {
	    newOut = in.reverse;
	    newIn = Nil;
	} else {
	    newOut = out;
	    newIn = in;
	}
	if (newOut.isEmpty)
  	  error("queue empty");
        else {
            Pair(newOut.head,new Queue(newIn,newOut.tail));
        }
    }

   /** Returns the first element in the queue, or throws an error if there
     *  is no element contained in the queue.
     *
     *  @returns the first element.
     */
    def front: A = {
	if (out.isEmpty) {
	    if (in.isEmpty) {
		error("queue empty");
	    } else {in.last;}
	} else
	out.head;
    }

  /** Returns a string representation of this queue. The resulting string
  * begins with the string <code>start</code> and is finished by the string
  * <code>end</code>. Inside, the string representations of elements (w.r.t.
  * the method <code>toString()</code>) are separated by the string
  * <code>sep</code>.
  * <p>
  * Ex: <br>
  * <code>Queue(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"</code>
  * @param start starting string.
  * @param sep separator string.
  * @param end ending string.
  * @return a string representation of this list.
  */
  def mkString(start: String, sep: String, end: String): String =
      (out:::(in.reverse)).mkString(start,sep,end);

  /** Returns a string representation of this queue.
   */
  override def toString() = (out:::(in.reverse)).mkString("Queue(", ",", ")");

  /** Compares two queues for equality by comparing
   *  each element in the queues.
   */
  override def equals(o :Any) = {
      /* Make sure o is a Queue. */
      if (o is Queue[Any]) {
	  /* o is a queue so we cast it and store it in q. */
	  val q:Queue[Any] = o as Queue[Any];
	  /* A function that compares the element at
	     position index in q with the element at
	     the same position in this (queue).
	     If they are equal the next element is
	     compared.
	  */
	  def eqe(index: int):Boolean = {
	      /* If all elements are compared
		 the queues are equal. */
	      index >= this.length ||
	      /* Otherwise: compare the elements */
	      (q.apply(index) == this.apply(index) &&
	       /* if they are equal compare the rest. */
	       eqe(index+1))
	  }
	  /* If the length of the ques are the same,
	     compare each element, starting at index 0. */
	  (q.length == this.length) && eqe(0);

      } else false; /* o is not a queue: not equal to this. */
  }
}


