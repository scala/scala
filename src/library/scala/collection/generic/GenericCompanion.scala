/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package generic

import mutable.Builder
import scala.language.higherKinds

/** A template class for companion objects of "regular" collection classes
 *  represent an unconstrained higher-kinded type. Typically
 *  such classes inherit from trait `GenericTraversableTemplate`.
 *  @tparam  CC   The type constructor representing the collection class.
 *  @see [[scala.collection.generic.GenericTraversableTemplate]]
 *  @author Martin Odersky
 *  @since 2.8
 *  @define coll  collection
 *  @define Coll  `CC`
 */
abstract class GenericCompanion[+CC[X] <: GenTraversable[X]] {
  /** The underlying collection type with unknown element type */
  protected[this] type Coll = CC[_]

  /** The default builder for `$Coll` objects.
   *  @tparam A      the type of the ${coll}'s elements
   */
  def newBuilder[A]: Builder[A, CC[A]]

  /** An empty collection of type `$Coll[A]`
   *  @tparam A      the type of the ${coll}'s elements
   */
  def empty[A]: CC[A] = {
    if ((this eq immutable.Seq) || (this eq collection.Seq)) Nil.asInstanceOf[CC[A]]
    else newBuilder[A].result()
  }

  /** Creates a $coll with the specified elements.
   *  @tparam A      the type of the ${coll}'s elements
   *  @param elems  the elements of the created $coll
   *  @return a new $coll with elements `elems`
   */
  def apply[A](elems: A*): CC[A] = {
    if (elems.isEmpty) empty[A]
    else {
      val b = newBuilder[A]
      b ++= elems
      b.result()
    }
  }
}
