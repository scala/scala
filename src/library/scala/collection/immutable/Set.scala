/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable


/** <p>
 *    This class represents immutable sets. Concrete set implementations
 *    just have to provide functionality for the abstract methods in
 *    <code>scala.collection.Set</code> as well as for <code>+</code> and
 *    <code>-</code>.
 *  </p>
 *  <p>
 *    Note that abstract immutable.Set's are not covariant in their type
 *    parameter.  This is because some subclasses cannot support the
 *    <code>+</code> method for arbitrary types.
 *  </p>
 *
 *  @author  Matthias Zenger, Martin Odersky
 *  @version 1.1, 03/05/2004
 */
object Set {
  /** The empty set of this type
   */
  def empty[A]: Set[A] = new EmptySet[A]

  /** The canonical factory for this type
   */
  def apply[A](elems: A*) = empty[A] ++ elems
}

trait Set[A] extends AnyRef with collection.Set[A] {

  /** @return an empty set of arbitrary element type
   */
  def empty[B]: Set[B]

  /** Create a new set with an additional element.
   */
  def +(elem: A): Set[A]

  /** Add two or more elements to this set.
   *  @param    elem1 the first element.
   *  @param    elem2 the second element.
   *  @param    elems the remaining elements.
   *  @return a new set with the elements added.
   */
  def + (elem1: A, elem2: A, elems: A*): Set[A] =
    this + elem1 + elem2 ++ elems

  /** Add all the elements provided by an iterator
   *  of the iterable object <code>elems</code> to the set.
   *
   *  @param elems  the iterable object containing the elements to be added
   *  @return a new set with the elements added.
   */
  def ++ (elems: Iterable[A]): Set[A] =
    (this /: elems) ((s, elem) => s + elem)

  /** Add all the elements provided by an iterator to the set.
   *  @param elems  the iterator containing the elements to be added
   *  @return a new set with the elements added.
   */
  def ++ (elems: Iterator[A]): Set[A] =
    (this /: elems) ((s, elem) => s + elem)

  /** <code>incl</code> can be used to add many elements to the set
   *  at the same time.
   */
  @deprecated
  def incl(elems: A*): Set[A] = incl(elems)

  /** This method will add all the elements provided by an iterator
   *  of the iterable object <code>that</code> to the set.
   *
   *  @param that ...
   */
  @deprecated
  def incl(that: Iterable[A]): Set[A] =
    that.foldLeft(this)((set, elem) => set + elem)

  /** Remove a single element from a set.
   *  @param elem the element to be removed
   *  @return a new set with the element removed.
   */
  def -(elem: A): Set[A]

  /** Remove two or more elements from this set.
   *  @param    elem1 the first element.
   *  @param    elem2 the second element.
   *  @param    elems the remaining elements.
   *  @return a new set with the elements removed.
   */
  def - (elem1: A, elem2: A, elems: A*): Set[A] =
    this - elem1 - elem2 -- elems

  /** Remove all the elements provided by an iterator
   *  of the iterable object <code>elems</code> from the set.
   *
   *  @param elems An iterable object containing the elements to remove from the set.
   *  @return a new set with the elements removed.
   */
  def -- (elems: Iterable[A]): Set[A] = this -- elems.elements

  /** Remove all the elements provided by an iterator
   *  <code>elems</code> from the set.
   *  @param elems An iterator containing the elements to remove from the set.
   *  @return a new set with the elements removed.
   */
  def -- (elems: Iterator[A]): Set[A] =
    (this /: elems) ((s, elem) => s - elem)

  /** <code>excl</code> removes many elements from the set.
   */
  @deprecated
  def excl(elems: A*): Set[A] = excl(elems)

  /** This method removes all the elements provided by an iterator
   *  of the iterable object <code>that</code> from the set.
   */
  @deprecated
  def excl(that: Iterable[A]): Set[A] =
    that.foldLeft(this)((set, elem) => set - elem)

  /** This method computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with
   */
  def intersect(that: collection.Set[A]): Set[A] = filter(that.contains)

  /** This method is an alias for <code>intersect</code>.
   *  It computes an intersection with set <code>that</code>.
   *  It removes all the elements that are not present in <code>that</code>.
   *
   *  @param that the set to intersect with
   */
   def ** (that: collection.Set[A]): Set[A] = intersect(that)

  /** Returns the set resulting from applying the given function <code>f</code> to each
   *  element of this set.
   *
   *  @param f function to apply to each element.
   *  @return a set containing <code>f(a0), ..., f(an)</code>
   *          if this set contains <code>a0, ..., an</code>.
   */
  override def map[B](f: A => B): Set[B] =
    foldLeft(empty[B])((set: Set[B], elem: A) => set + f(elem))

  /** Applies the given function <code>f</code> to each element of
   *  this set, then forms the union of all results.
   *  @param f function to apply to each element.
   *  @return a set containing all elements in each <code>f(a0), ..., f(an)</code>
   *          if this set contains <code>a0, ..., an</code>.
   */
  override def flatMap[B](f: A => Iterable[B]): Set[B] =
    foldLeft(empty[B])((set: Set[B], elem: A) => set incl f(elem))

  /** Method <code>filter</code> removes all elements from the set for
   *  which the predicate <code>p</code> yields the value <code>false</code>.
   *
   *  @param p The predicate used to filter the set
   */
  override def filter(p: A => Boolean): Set[A] =
    foldLeft(this)((set, elem) => if (p(elem)) set else set - elem)

}
