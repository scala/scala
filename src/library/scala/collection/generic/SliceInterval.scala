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

/** A container for the endpoints of a collection slice.
 *  The constructor is private to enforce the invariants:
 *  from >= 0, until >= 0, from <= until.
 */
private[collection] class SliceInterval private (val from: Int, val until: Int) {
  // The width of this slice from end to end.  This is the
  // maximum size of the collection slice, but the collection
  // need not have this many (or any) elements.  Since
  // from <= until is a constructor invariant, we don't have to
  // check for negative values.
  def width = until - from

  /** Returns a new SliceInterval with endpoints calculated in
   *  terms of the original collection.
   *  Example:
   *  {{{
   *  val coll = (1 to 100).view.slice(10, 30).slice(1, 3)
   *  // the second call to slice causes the interval to
   *  // be recalculated: the result is SliceInterval(11, 13).
   *  }}}
   */
  def recalculate(_from: Int, _until: Int): SliceInterval = {
    val lo    = _from max 0
    val elems = scala.math.min(_until - lo, width)
    val start = from + lo

    if (elems <= 0) new SliceInterval(from, from)
    else new SliceInterval(start, start + elems)
  }
  def recalculate(interval: SliceInterval): SliceInterval =
    recalculate(interval.from, interval.until)
}

object SliceInterval {
  def apply(from: Int, until: Int) = {
    val lo = from max 0
    val hi = until max 0

    if (hi <= lo) new SliceInterval(lo, lo)
    else new SliceInterval(lo, hi)
  }
}
