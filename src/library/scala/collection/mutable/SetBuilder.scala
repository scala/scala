/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package mutable

import generic._

/** The canonical builder for mutable Sets.
 *
 *  @tparam A      The type of the elements that will be contained in this set.
 *  @tparam Coll   The type of the actual collection this set builds.
 *  @param empty   The empty element of the collection.
 *  @since 2.8
 */
class SetBuilder[A, Coll <: Addable[A, Coll] with collection.Iterable[A] with collection.IterableLike[A, Coll]](empty: Coll)
extends AddingBuilder[A, Coll](empty) { }
