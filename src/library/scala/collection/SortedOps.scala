/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection


/** Base trait for sorted collections */
trait SortedOps[A, +C] {

  def ordering: Ordering[A]

  /** Returns the first key of the collection. */
  def firstKey: A

  /** Returns the last key of the collection. */
  def lastKey: A

  /** Comparison function that orders keys. */
  @deprecated("Use ordering.compare instead", "2.13.0")
  @deprecatedOverriding("Use ordering.compare instead", "2.13.0")
  @inline def compare(k0: A, k1: A): Int = ordering.compare(k0, k1)

  /** Creates a ranged projection of this collection. Any mutations in the
    *  ranged projection will update this collection and vice versa.
    *
    *  Note: keys are not guaranteed to be consistent between this collection
    *  and the projection. This is the case for buffers where indexing is
    *  relative to the projection.
    *
    *  @param from  The lower-bound (inclusive) of the ranged projection.
    *               `None` if there is no lower bound.
    *  @param until The upper-bound (exclusive) of the ranged projection.
    *               `None` if there is no upper bound.
    */
  def rangeImpl(from: Option[A], until: Option[A]): C

  /** Creates a ranged projection of this collection with both a lower-bound
    *  and an upper-bound.
    *
    *  @param from The lower-bound (inclusive) of the ranged projection.
    *  @param until The upper-bound (exclusive) of the ranged projection.
    */
  def range(from: A, until: A): C = rangeImpl(Some(from), Some(until))

  /** Creates a ranged projection of this collection with no upper-bound.
    *
    *  @param from The lower-bound (inclusive) of the ranged projection.
    */
  @deprecated("Use rangeFrom", "2.13.0")
  final def from(from: A): C = rangeFrom(from)

  /** Creates a ranged projection of this collection with no upper-bound.
   *
   *  @param from The lower-bound (inclusive) of the ranged projection.
   */
  def rangeFrom(from: A): C = rangeImpl(Some(from), None)

  /** Creates a ranged projection of this collection with no lower-bound.
    *
    *  @param until The upper-bound (exclusive) of the ranged projection.
    */
  @deprecated("Use rangeUntil", "2.13.0")
  final def until(until: A): C = rangeUntil(until)

  /** Creates a ranged projection of this collection with no lower-bound.
   *
   *  @param until The upper-bound (exclusive) of the ranged projection.
   */
  def rangeUntil(until: A): C = rangeImpl(None, Some(until))

  /** Create a range projection of this collection with no lower-bound.
    *  @param to The upper-bound (inclusive) of the ranged projection.
    */
  @deprecated("Use rangeTo", "2.13.0")
  final def to(to: A): C = rangeTo(to)

  /** Create a range projection of this collection with no lower-bound.
    *  @param to The upper-bound (inclusive) of the ranged projection.
    */
  def rangeTo(to: A): C
}
