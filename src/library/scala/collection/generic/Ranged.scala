/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Ranged.scala 17537 2009-04-20 18:37:37Z odersky $

package scala.collection.generic

/** Any collection (including maps) whose keys (or elements) are ordered.
 *
 *  @author martin Odersky
 *  @version 2.8
 */
trait Ranged[K, +This <: Ranged[K, This]] {

  /** Returns the first key of the collection. */
  def firstKey: K

  /** Returns the last key of the collection. */
  def lastKey: K

  /** Comparison function that orders keys. */
  def compare(k0: K, k1: K): Int

  /** Creates a ranged projection of this collection. Any mutations in the
   *  ranged projection will update this collection and vice versa.  Note: keys
   *  are not garuanteed to be consistent between this collection and the projection.
   *  This is the case for buffers where indexing is relative to the projection.
   *
   *  @param from  The lower-bound (inclusive) of the ranged projection.
   *               <code>None</code> if there is no lower bound.
   *  @param until The upper-bound (exclusive) of the ranged projection.
   *               <code>None</code> if there is no upper bound.
   */
  def rangeImpl(from: Option[K], until: Option[K]): This

  /** Creates a ranged projection of this collection with no upper-bound.
   *
   *  @param from The lower-bound (inclusive) of the ranged projection.
   */
  def from(from: K): This = rangeImpl(Some(from), None)

  /** Creates a ranged projection of this collection with no lower-bound.
   *
   *  @param until The upper-bound (exclusive) of the ranged projection.
   */
  def until(until: K): This = rangeImpl(None, Some(until))

  /** Creates a ranged projection of this collection with both a lower-bound
   *  and an upper-bound.
   *
   *  @param from  The upper-bound (exclusive) of the ranged projection.
   *  @param until ...
   *  @return      ...
   */
  def range(from: K, until: K): This = rangeImpl(Some(from), Some(until))
}
