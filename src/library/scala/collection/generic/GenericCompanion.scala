/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package generic

import mutable.Builder

/** A template class for companion objects of ''regular'' collection classes
 *  represent an unconstrained higher-kinded type. Typically
 *  such classes inherit from trait `GenericTraversableTemplate`.
 *  @tparam  CC   The type constructor representing the collection class.
 *  @see GenericTraversableTemplate
 *  @author Martin Odersky
 *  @since 2.8
 *  @define coll  collection
 *  @define Coll  CC
 */
abstract class GenericCompanion[+CC[X] <: Traversable[X]] {
  /** The underlying collection type with unknown element type */
  type Coll = CC[_]

  /** The default builder for `$Coll` objects. */
  def newBuilder[A]: Builder[A, CC[A]]

  /** The empty collection of type `$Coll[A]` */
  def empty[A]: CC[A] = newBuilder[A].result

  /** Creates a $coll with the specified elements.
   *  @param elems  the elements of the created $coll
   *  @return a new $coll with elements `elems`
   */
  def apply[A](elems: A*): CC[A] = {
    val b = newBuilder[A]
    b ++= elems
    b.result
  }
}
