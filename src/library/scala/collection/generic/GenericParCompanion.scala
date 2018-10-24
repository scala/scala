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

import scala.collection.parallel.Combiner
import scala.collection.parallel.ParIterable
import scala.collection.parallel.ParMap
import scala.language.higherKinds

/** A template class for companion objects of parallel collection classes.
 *  They should be mixed in together with `GenericCompanion` type.
 *
 *  @define Coll `ParIterable`
 *  @tparam CC   the type constructor representing the collection class
 *  @since 2.8
 */
trait GenericParCompanion[+CC[X] <: ParIterable[X]] {
  /** The default builder for $Coll objects.
   */
  def newBuilder[A]: Combiner[A, CC[A]]

  /** The parallel builder for $Coll objects.
   */
  def newCombiner[A]: Combiner[A, CC[A]]
}

trait GenericParMapCompanion[+CC[P, Q] <: ParMap[P, Q]] {
  def newCombiner[P, Q]: Combiner[(P, Q), CC[P, Q]]
}

