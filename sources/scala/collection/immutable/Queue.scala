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
    /*    val in:List[A]= Nil;
	  val out:List[A]= Nil; */

    def at(n: Int): A =
	if (n < out.length) out.at(n)
	    else (in.reverse).at(n - out.length);

    def apply(n: Int): A = at(n);

    /** Returns the elements in the list as an iterator
     */
    def elements: Iterator[A] = (out:::(in.reverse)).elements;

    /** Checks if the queue is empty.
     *
     *  @returns true, iff there is no element in the queue.
     */
    def isEmpty: Boolean = (in.isEmpty && out.isEmpty);

    def length = in.length + out.length;

    /** Creates a new queue with element added at the end
     *  of the old queue.
     *
     *  @param  elem        the element to insert
     */
    def +(elem: A):Queue[A] = {
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
    def +(iter: Iterable[A]) = {
	var q:List[A] = in;
	iter.elements.foreach(e => q = (e::q));
	new Queue(q,out);
    }

    /** Returns a new queue with all elements added.
     *
     *  @param  elems		the elements to add.
     */
    def enqueue(elems: A*): Queue[A] = (this + elems);

    /** Returns a tuple with the first element in the queue,
     *  and a new queu with this element removed.
     *
     *  @returns the first element of the queue.
     */
    def dequeue: Tuple2[A,Queue[A]] = {
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
            Tuple2(newOut.head,new Queue(newIn,newOut.tail));
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
}


