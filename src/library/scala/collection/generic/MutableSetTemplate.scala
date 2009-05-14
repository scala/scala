/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map.scala 16884 2009-01-09 16:52:09Z cunei $


package scala.collection.generic

/** A generic template for mutable sets of elements of type A.
 *  To implement a concrete mutable set, you need to provide implementations of the following methods:
 *
 *  def contains(elem: A): Boolean
 *  def elements: Iterator[A]
 *  def += (elem: A): this.type
 *  def -= (elem: A): this.type
 *
 * If you wish that methods like, take, drop, filter return the same kind of map, you should also
 * override:
 *
 *   def empty: This
 *
 * It is also good idea to override methods `foreach` and `size` for efficiency.
 *
 */
trait MutableSetTemplate[A, +This <: MutableSetTemplate[A, This] with mutable.Set[A]]
  extends SetTemplate[A, This]
     with Builder[A, This]
     with Growable[A]
     with Shrinkable[A]
     with Cloneable[This]
{ self =>

  override protected[this] def newBuilder: Builder[A, This] = empty

  /** Adds a new element to the set.
   *
   *  @param elem the element to be added
   *  @return true if the element was already present in the set.
   */
  def put(elem: A): Boolean = {
    val r = contains(elem)
    this += elem
    r
  }

  /** Removes a single element from a set.
   *  @param elem  The element to be removed.
   *  @return  true if the element was already present in the set.
   */
  def remove(elem: A): Boolean = {
    val r = contains(elem)
    this -= elem
    r
  }

  /** This method allows one to add or remove an element <code>elem</code>
   *  from this set depending on the value of parameter <code>included</code>.
   *  Typically, one would use the following syntax:
   *  <pre>set(elem) = true</pre>
   *
   */
  def update(elem: A, included: Boolean) {
    if (included) this += elem else this -= elem
  }



  /** Adds a new element to the set.
   *
   *  @param elem the element to be added
   */
  def +=(elem: A): this.type

  /** Removes a single element from a set.
   *  @param elem  The element to be removed.
   */
  def -=(elem: A): this.type

  /** Removes all elements from the set for which the predicate <code>p</code>
   *  yields the value <code>false</code>.
   */
  def retain(p: A => Boolean): Unit = for (elem <- this.toList) if (!p(elem)) this -= elem

  /** Removes all elements from the set. After this operation is completed,
   *  the set will be empty.
   */
  def clear() { foreach(-=) }

  override def clone(): This = empty ++= thisCollection

  def result: This = thisCollection

  /** Adds a single element to this collection and returns
   *  the collection itself.
   *
   *  @param elem  the element to add.
   *  @deprecated  use += instead if you intend to add by side effect to an existing collection.
   *               Use `clone() +=` if you intend to create a new collection.
   */
  @deprecated override def + (elem: A): This = { +=(elem); thisCollection }

  /** Adds two or more elements to this collection and returns
   *  the collection itself.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @deprecated  use += instead if you intend to add by side effect to an existing collection.
   *               Use `clone() +=` if you intend to create a new collection.
   */
  @deprecated override def + (elem1: A, elem2: A, elems: A*): This = {
    this += elem1 += elem2 ++= elems
    thisCollection
  }

  /** Adds a number of elements provided by a traversable object and returns
   *  either the collection itself.
   *
   *  @param iter     the iterable object.
   *  @deprecated  use ++= instead if you intend to add by side effect to an existing collection.
   *               Use `clone() ++=` if you intend to create a new collection.
   */
  @deprecated override def ++(iter: Traversable[A]): This = {
    for (elem <- iter) +=(elem)
    thisCollection
  }


  /** Adds a number of elements provided by an iterator and returns
   *  the collection itself.
   *
   *  @param iter   the iterator
   *  @deprecated  use ++= instead if you intend to add by side effect to an existing collection.
   *               Use `clone() ++=` if you intend to create a new collection.
   */
  @deprecated override def ++ (iter: Iterator[A]): This = {
    for (elem <- iter) +=(elem)
    thisCollection
  }

  /** Removes a single element from this collection and returns
   *  the collection itself.
   *
   *  @param elem  the element to remove.
   *  @deprecated  use -= instead if you intend to remove by side effect from an existing collection.
   *               Use `clone() -=` if you intend to create a new collection.
   */
  @deprecated override def -(elem: A): This = { -=(elem); thisCollection }

  /** Removes two or more elements from this collection and returns
   *  the collection itself.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @deprecated  use -= instead if you intend to remove by side effect from an existing collection.
   *               Use `clone() -=` if you intend to create a new collection.
   */
  @deprecated override def -(elem1: A, elem2: A, elems: A*): This = {
    this -= elem1 -= elem2 --= elems
    thisCollection
  }

  /** Removes a number of elements provided by a traversible object and returns
   *  the collection itself.
   *  @deprecated  use --= instead if you intend to remove by side effect from an existing collection.
   *               Use `clone() --=` if you intend to create a new collection.
   *
   *  @param iter     the iterable object.
   */
  @deprecated override def --(iter: Traversable[A]): This = {
    for (elem <- iter) -=(elem)
    thisCollection
  }

  /** Removes a number of elements provided by an iterator and returns
   *  the collection itself.
   *
   *  @param iter   the iterator
   *  @deprecated  use --= instead if you intend to remove by side effect from an existing collection.
   *               Use `clone() --=` if you intend to create a new collection.
   */
  @deprecated override def --(iter: Iterator[A]): This = {
    for (elem <- iter) -=(elem)
    thisCollection
  }

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   *  @throws <code>Predef.UnsupportedOperationException</code>
   *  if the message was not understood.
   def <<(cmd: Message[A]): Unit = cmd match {
    case Include(elem) => this += elem
    case Remove(elem) => this -= elem
    case Reset() => clear
    case s: Script[_] => s.elements foreach <<
    case _ => throw new UnsupportedOperationException("message " + cmd + " not understood")
  }
  */
}






