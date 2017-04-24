/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

import generic._
import script._
import scala.annotation.migration
import parallel.mutable.ParSet

/** A template trait for mutable sets of type `mutable.Set[A]`.
 *
 *    This trait provides most of the operations of a `mutable.Set` independently of its representation.
 *    It is typically inherited by concrete implementations of sets.
 *
 *  $setNote
 *
 *  @tparam A    the type of the elements of the set
 *  @tparam This the type of the set itself.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since 2.8
 *
 *  @define setNote
 *
 *    To implement a concrete mutable set, you need to provide implementations
 *    of the following methods:
 *    {{{
 *       def contains(elem: A): Boolean
 *       def iterator: Iterator[A]
 *       def += (elem: A): this.type
 *       def -= (elem: A): this.type
 *    }}}
 *    If you wish that methods like `take`,
 *    `drop`, `filter` return the same kind of set,
 *    you should also override:
 *    {{{
 *       def empty: This
 *    }}}
 *    It is also good idea to override methods `foreach` and
 *    `size` for efficiency.
 *  @define addDuplicates
 *    Note that duplicates (elements for which `equals` yields true) will be
 *    removed, but it is not specified whether it will be an element of this
 *    set or a newly added element.
 *  @define coll mutable set
 *  @define Coll mutable.Set
 */
trait SetLike[A, +This <: SetLike[A, This] with Set[A]]
  extends scala.collection.SetLike[A, This]
     with Scriptable[A]
     with Builder[A, This]
     with Growable[A]
     with Shrinkable[A]
     with Cloneable[mutable.Set[A]]
     with Parallelizable[A, ParSet[A]]
{ self =>

  /** A common implementation of `newBuilder` for all mutable sets
   *  in terms of `empty`. Overrides the implementation in `collection.SetLike`
   *  for better efficiency.
   */
  override protected[this] def newBuilder: Builder[A, This] = empty

  protected[this] override def parCombiner = ParSet.newCombiner[A]

  /** Converts this $coll to a sequence.
    *
    * ```Note```: assumes a fast `size` method.  Subclasses should override if this is not true.
    */
  override def toSeq: collection.Seq[A] = {
    // ArrayBuffer for efficiency, preallocated to the right size.
    val result = new ArrayBuffer[A](size)
    foreach(result += _)
    result
  }

  /** Adds an element to this $coll.
   *
   *  @param elem the element to be added
   *  @return `true` if the element was not yet present in the set, `false` otherwise.
   */
  def add(elem: A): Boolean = {
    val r = contains(elem)
    this += elem
    !r
  }

  /** Removes an element from this set.
   *
   *  @param elem  The element to be removed.
   *  @return  `true` if the element was previously present in the set, `false` otherwise.
   */
  def remove(elem: A): Boolean = {
    val r = contains(elem)
    this -= elem
    r
  }

  /** Updates the presence of a single element in this set.
   *
   * This method allows one to add or remove an element `elem`
   *  from this set depending on the value of parameter `included`.
   *  Typically, one would use the following syntax:
   *  {{{
   *     set(elem) = true  // adds element
   *     set(elem) = false // removes element
   *  }}}
   *
   *  @param elem     the element to be added or removed
   *  @param included a flag indicating whether element should be included or excluded.
   */
  def update(elem: A, included: Boolean) {
    if (included) this += elem else this -= elem
  }

  // abstract methods from Growable/Shrinkable

  /** Adds a single element to the set. */
  def +=(elem: A): this.type
  def -=(elem: A): this.type

  /** Removes all elements from the set for which do not satisfy a predicate.
   *  @param  p  the predicate used to test elements. Only elements for
   *             which `p` returns `true` are retained in the set; all others
   *             are removed.
   */
  def retain(p: A => Boolean): Unit =
    for (elem <- this.toList) // scala/bug#7269 toList avoids ConcurrentModificationException
      if (!p(elem)) this -= elem

  /** Removes all elements from the set. After this operation is completed,
   *  the set will be empty.
   */
  def clear(): Unit =
    for (elem <- this.toList)
      this -= elem

  override def clone(): This = empty ++= repr.seq

  /** The result when this set is used as a builder
   *  @return  the set representation itself.
   */
  def result: This = repr

  /** Creates a new set consisting of all the elements of this set and `elem`.
   *
   *  $addDuplicates
   *
   *  @param elem  the element to add.
   *  @return      a new set consisting of elements of this set and `elem`.
   */
  @migration("`+` creates a new set. Use `+=` to add an element to this set and return that set itself.", "2.8.0")
  override def + (elem: A): This = clone() += elem

  /** Creates a new set consisting of all the elements of this set and two or more
   *  specified elements.
   *
   *  $addDuplicates
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return      a new set consisting of all the elements of this set, `elem1`,
   *               `elem2` and those in `elems`.
   */
  @migration("`+` creates a new set. Use `+=` to add an element to this set and return that set itself.", "2.8.0")
  override def + (elem1: A, elem2: A, elems: A*): This =
    clone() += elem1 += elem2 ++= elems

  /** Creates a new set consisting of all the elements of this set and those
   *  provided by the specified traversable object.
   *
   *  $addDuplicates
   *
   *  @param xs        the traversable object.
   *  @return          a new set consisting of elements of this set and those in `xs`.
   */
  @migration("`++` creates a new set. Use `++=` to add elements to this set and return that set itself.", "2.8.0")
  override def ++(xs: GenTraversableOnce[A]): This = clone() ++= xs.seq

  /** Creates a new set consisting of all the elements of this set except `elem`.
   *
   *  @param elem  the element to remove.
   *  @return      a new set consisting of all the elements of this set except `elem`.
   */
  @migration("`-` creates a new set. Use `-=` to remove an element from this set and return that set itself.", "2.8.0")
  override def -(elem: A): This = clone() -= elem

  /** Creates a new set consisting of all the elements of this set except the two
   *  or more specified elements.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @return      a new set consisting of all the elements of this set except
   *               `elem1`, `elem2` and `elems`.
   */
  @migration("`-` creates a new set. Use `-=` to remove an element from this set and return that set itself.", "2.8.0")
  override def -(elem1: A, elem2: A, elems: A*): This =
    clone() -= elem1 -= elem2 --= elems

  /** Creates a new set consisting of all the elements of this set except those
   *  provided by the specified traversable object.
   *
   *  @param xs       the traversable object.
   *  @return         a new set consisting of all the elements of this set except
   *                  elements from `xs`.
   */
  @migration("`--` creates a new set. Use `--=` to remove elements from this set and return that set itself.", "2.8.0")
  override def --(xs: GenTraversableOnce[A]): This = clone() --= xs.seq

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   *  @throws UnsupportedOperationException
   *  if the message was not understood.
   */
  @deprecated("scripting is deprecated", "2.11.0")
  def <<(cmd: Message[A]): Unit = cmd match {
    case Include(_, x)     => this += x
    case Remove(_, x)      => this -= x
    case Reset()           => clear()
    case s: Script[_]      => s.iterator foreach <<
    case _                 => throw new UnsupportedOperationException("message " + cmd + " not understood")
  }
}
