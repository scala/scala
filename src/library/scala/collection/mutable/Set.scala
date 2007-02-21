/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This class represents mutable sets. Concrete set implementations
 *  just have to provide functionality for the abstract methods in
 *  <a href="../Set.html" target="contentFrame">
 *  <code>scala.collection.Set</code></a> as well as for <code>+=</code>,
 *  <code>-= and <code>clear</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 09/05/2004
 */
object Set {

  /** The empty map of this type; this is implemented as a hashtable */
  def empty[A]: Set[A] = new HashSet[A]

  /** The canonical factory for this type
   */
  def apply[A](elems: A*) = empty[A] ++ elems
}

@cloneable
trait Set[A] extends collection.Set[A] with Scriptable[Message[A]] {

  /** This method allows one to add or remove an element <code>elem</code>
   *  from this set depending on the value of parameter <code>included</code>.
   *  Typically, one would use the following syntax:
   *  <pre>set(elem) = true</pre>
   *
   */
  def update(elem: A, included: Boolean): Unit =
    if (included) this += elem else this -= elem

  /** Add a new element to the set.
   *
   *  @param elem the element to be added
   */
  def +=(elem: A)

  /** Add two or more elements to this set.
   *  @param    elem1 the first element.
   *  @param    kv2 the second element.
   *  @param    kvs the remaining elements.
   */
  def += (elem1: A, elem2: A, elems: A*) { this += elem1; this += elem2; this ++= elems }

  /** Add all the elements provided by an iterator
   *  of the iterable object <code>that</code> to the set.
   *  @param elems  the iterable object containing the elements to be added
   */
  def ++=(elems: Iterable[A]): Unit = ++=(elems.elements)

  /** Add all the elements provided by an iterator
   *  <code>elems</code> to the set.
   *  @param elems  the iterator containing the elements to be added
   */
  def ++=(elems: Iterator[A]): Unit = elems foreach +=

  /** Add a new element to the set.
   *  @return the set itself with the element added.
   *
   *  @param elem the element to be added
   */
  def + (elem: A): Set[A] = { +=(elem); this }

  /** Add two or more elements to this set.
   *  @param    elem1 the first element.
   *  @param    kv2 the second element.
   *  @param    kvs the remaining elements.
   *  @return the set itself with the elements added.
   */
  def + (elem1: A, elem2: A, elems: A*): Set[A] = { this.+=(elem1, elem2, elems: _*); this }

  /** Add all the elements provided by an iterator
   *  of the iterable object <code>elems</code> to the set.
   *
   *  @param elems  the iterable object containing the elements to be added
   *  @return the set itself with the elements added.
   */
  def ++ (elems: Iterable[A]): Set[A] = { this ++= elems; this }

  /** Add all the elements provided by an iterator
   *  <code>elems</code> to the set.
   *  @param elems  the iterator containing the elements to be added
   */
  def ++ (elems: Iterator[A]): Set[A] = { this ++= elems; this }

  /** <code>incl</code> can be used to add many elements to the set
   *  at the same time.
   *  @deprecated   use <code>++=</code> instead
   */
  @deprecated
  def incl(elems: A*): Unit = ++=(elems.elements)

  /** Removes a single element from a set.
   *  @param elem  The element to be removed.
   */
  def -=(elem: A)

  /** Remove two or more elements from this set.
   *  @param    elem1 the first element.
   *  @param    elem2 the second element.
   *  @param    elems the remaining elements.
   */
  def -= (elem1: A, elem2: A, elems: A*) { this -= elem1; this -= elem2; this --= elems }

  /** Remove all the elements provided by an iterator
   *  of the iterable object <code>elems</code> from the set.
   */
  def --=(elems: Iterable[A]): Unit = --=(elems.elements)

  /** Remove all the elements provided by an iterator
   *  <code>elems</code> from the set.
   */
  def --=(elems: Iterator[A]): Unit = elems foreach -=

  /** Remove a new element from the set.
   *  @return the set itself with the element removed.
   *
   *  @param elem the element to be removed
   */
  def - (elem: A): Set[A] = { -=(elem); this }

  /** Remove two or more elements from this set.
   *  @param    elem1 the first element.
   *  @param    elem2 the second element.
   *  @param    elems the remaining elements.
   *  @return the set itself with the elements removed.
   */
  def - (elem1: A, elem2: A, elems: A*): Set[A] = { this.-=(elem1, elem2, elems: _*); this }

  /** Remove all the elements provided by an iterator
   *  of the iterable object <code>elems</code> from the set.
   *
   *  @param elems An iterable object containing the elements to remove from the set.
   *  @return the set itself with the elements removed.
   */
  def -- (elems: Iterable[A]): Set[A] = { this --= elems; this }

  /** Remove all the elements provided by an iterator
   *  <code>elems</code> from the set.
   *  @param elems An iterator containing the elements to remove from the set.
   *  @return the set itself with the elements removed.
   */
  def -- (elems: Iterator[A]): Set[A] = { this --= elems; this }

  /** <code>excl</code> removes many elements from the set.
   */
  @deprecated
  def excl(elems: A*): Unit = --=(elems.elements)

  /** This method computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with.
   */
  def intersect(that: Set[A]): Unit = retain(that.contains)

  /** Method <code>retain removes all elements from the set for
   *  which the predicate <code>p</code> yields the value <code>false</code>.
   */
  def retain(p: A => Boolean): Unit = toList.foreach {
    elem => if (!p(elem)) -=(elem)
  }

  /** Removes all elements from the set. After this operation is completed,
   *  the set will be empty.
   */
  def clear(): Unit = elements foreach -=

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   *  @throws <code>Predef.UnsupportedOperationException</code>
   *  if the message was not understood.
   */
  def <<(cmd: Message[A]): Unit = cmd match {
    case Include(elem) => this += elem
    case Remove(elem) => this -= elem
    case Reset() => clear
    case s: Script[_] => s.elements foreach <<
    case _ => throw new UnsupportedOperationException("message " + cmd + " not understood")
  }

  /** Return a clone of this set.
   *
   *  @return  a set with the same elements.
   */
  override def clone(): Set[A] = super.clone().asInstanceOf[Set[A]]
}
