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

import parallel.Combiner
import scala.collection.IterableOnce

/** This trait describes collections which can be turned into parallel collections
 *  by invoking the method `par`. Parallelizable collections may be parameterized with
 *  a target type different than their own.
 *
 *  @tparam A            the type of the elements in the collection
 *  @tparam ParRepr      the actual type of the collection, which has to be parallel
 */
trait Parallelizable[+A, +ParRepr <: Parallel] extends Any {

  def seq: IterableOnceIterableOnce[A]

  /** Returns a parallel implementation of this collection.
   *
   *  For most collection types, this method creates a new parallel collection by copying
   *  all the elements. For these collection, `par` takes linear time. Mutable collections
   *  in this category do not produce a mutable parallel collection that has the same
   *  underlying dataset, so changes in one collection will not be reflected in the other one.
   *
   *  Specific collections (e.g. `ParArray` or `mutable.ParHashMap`) override this default
   *  behaviour by creating a parallel collection which shares the same underlying dataset.
   *  For these collections, `par` takes constant or sublinear time.
   *
   *  All parallel collections return a reference to themselves.
   *
   *  @return  a parallel implementation of this collection
   */
  def par: ParRepr = {
    val cb = parCombiner
    for (x <- seq) cb += x
    cb.result()
  }

  /** The default `par` implementation uses the combiner provided by this method
   *  to create a new parallel collection.
   *
   *  @return  a combiner for the parallel collection of type `ParRepr`
   */
  protected[this] def parCombiner: Combiner[A, ParRepr]
}

