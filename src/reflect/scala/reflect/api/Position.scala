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

package scala
package reflect
package api

import scala.reflect.macros.Attachments

/**
 *  <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  Position tracks the origin of [[Symbols#Symbol symbols]] and [[Trees#Tree tree nodes]]. They are commonly used when
 *  displaying warnings and errors, to indicate the incorrect point in the program.
 *
 *  Every non-empty position refers to a SourceFile and three character
 *  offsets within it: start, end, and point. The point is where the ^ belongs when
 *  issuing an error message, usually a Name. A range position can be designated
 *  as transparent, which excuses it from maintaining the invariants to follow. If
 *  a transparent position has opaque children, those are considered as if they were
 *  the direct children of the transparent position's parent.
 *
 *  Note: some of these invariants actually apply to the trees which carry
 *  the positions, but they are phrased as if the positions themselves were
 *  the parent/children for conciseness.
 *
 *  Invariant 1: in a focused/offset position, start == point == end
 *  Invariant 2: in a range position,          start <= point <  end
 *  Invariant 3: an offset position never has a child with a range position
 *  Invariant 4: every range position child of a range position parent is contained within its parent
 *  Invariant 5: opaque range position siblings overlap at most at a single point
 *
 *  The following tests are useful on positions:
 *
 *  pos.isDefined     true if position is not an UndefinedPosition (those being NoPosition and FakePos)
 *  pos.isRange       true if position is a range (opaque or transparent) which implies start < end
 *  pos.isOpaqueRange true if position is an opaque range
 *
 *  The following accessor methods are provided - an exception will be thrown if
 *  point/start/end are attempted on an UndefinedPosition.
 *
 *  pos.source       The source file of the position, or NoSourceFile if unavailable
 *  pos.point        The offset of the point
 *  pos.start        The (inclusive) start offset, or the point of an offset position
 *  pos.end          The (exclusive) end offset, or the point of an offset position
 *
 *  The following conversion methods are often used:
 *
 *  pos.focus           Converts a range position to an offset position focused on the point
 *  pos.makeTransparent Convert an opaque range into a transparent range
 *
 *  For more information about `Position`s, see the [[https://docs.scala-lang.org/overviews/reflection/annotations-names-scopes.html Reflection Guide: Annotations, Names, Scopes, and More]]
 *
 *  @groupname Common   Commonly used methods
 *  @group ReflectionAPI
 */
trait Position extends Attachments {

  /** @inheritdoc */
  type Pos >: Null <: AnyRef with Position

  ////////////////// POSITION FLAVORS //////////////////

  /** Is this position a range position? */
  def isRange: Boolean

  /** Is this position a transparent position? */
  def isTransparent: Boolean

  /** Is this position a non-transparent range position? */
  def isOpaqueRange: Boolean

  /** If this is a range position, the offset position of its point.
   *  Otherwise the position itself
   */
  def focus: Pos

  /** If opaque range, make this position transparent. */
  def makeTransparent: Pos

  ////////////////// POSITION ESSENTIALS //////////////////

  /** The start of the position's range, or the point if not a range position. */
  def start: Int

  /** The point (where the ^ is) of the position, which is easiest to access using the [[line]] and [[column]] values.
   *  The [[lineContent line content]] is also available.
   *  @group Common
   */
  def point: Int

  /** The end of the position's range, or the point if not a range position.
   */
  def end: Int

  /** Java file corresponding to the source file of this position.
   *
   *  The return type is `scala.reflect.io.AbstractFile`, which belongs to an experimental part of Scala reflection.
   *  It should not be used unless you know what you are doing. In subsequent releases, this API will be refined
   *  and exposed as a part of scala.reflect.api.
   *
   *  @group Common
   */
  def source: scala.reflect.internal.util.SourceFile

  /** The position indicates a [[column `column`]] and the `line` in the source file.
   *  @group Common
   */
  def line: Int

  /** The position indicates a `column` and the [[line `line`]] in the source file.
   *  @group Common
   */
  def column: Int

  ////////////////// POSITION FACTORIES //////////////////

  /** Returns a new position with the same attributes, but a different start value (if a range).
   */
  def withStart(off: Int): Pos

  /** Returns a new position with the same attributes, but a different end value (if a range).
   */
  def withEnd(off: Int): Pos

  /** Returns a new position with the same attributes, but a different point value (if a range or offset).
   */
  def withPoint(off: Int): Pos

  ////////////////// STUFF //////////////////

  /** Is this position not a NoPosition?
   *  If isDefined is true, offset and source are both defined.
   *  @group Common
   */
  @deprecated("removed from the public API", "2.11.0") def isDefined: Boolean

  /** The point (where the ^ is) of the position, or else `default` if undefined.
   *  @group Common
   */
  @deprecated("removed from the public API", "2.11.0") def pointOrElse(default: Int): Int

  /** The start of the position's range, or point if not a range position. */
  @deprecated("removed from the public API", "2.11.0") def startOrPoint: Int

  /** The end of the position's range, or point if not a range position.
   */
  @deprecated("removed from the public API", "2.11.0") def endOrPoint: Int

  /** If this is a range, the union with the other range, with the point of this position.
   *  Otherwise, this position
   */
  @deprecated("removed from the public API", "2.11.0") def union(pos: Pos): Pos

  /** If this is a range position, the offset position of its start.
   *  Otherwise the position itself
   */
  @deprecated("removed from the public API", "2.11.0") def focusStart: Pos

  /** If this is a range position, the offset position of its end.
   *  Otherwise the position itself
   */
  @deprecated("removed from the public API", "2.11.0") def focusEnd: Pos

  /** Does this position include the given position `pos`?
   *  This holds if `this` is a range position and its range [start..end]
   *  is the same or covers the range of the given position, which may or may not be a range position.
   */
  @deprecated("removed from the public API", "2.11.0") def includes(pos: Pos): Boolean

  /** Does this position properly include the given position `pos` ("properly" meaning their
   *  ranges are not the same)?
   */
  @deprecated("removed from the public API", "2.11.0") def properlyIncludes(pos: Pos): Boolean

  /** Does this position precede that position?
   *  This holds if both positions are defined and the end point of this position
   *  is not larger than the start point of the given position.
   */
  @deprecated("removed from the public API", "2.11.0") def precedes(pos: Pos): Boolean

  /** Does this position properly precede the given position `pos` ("properly" meaning their ranges
   *  do not share a common point).
   */
  @deprecated("removed from the public API", "2.11.0") def properlyPrecedes(pos: Pos): Boolean

  /** Does this position overlap with that position?
   *  This holds if both positions are ranges and there is an interval of
   *  non-zero length that is shared by both position ranges.
   */
  @deprecated("removed from the public API", "2.11.0") def overlaps(pos: Pos): Boolean

  /** Does this position cover the same range as that position?
   *  Holds only if both position are ranges
   */
  @deprecated("removed from the public API", "2.11.0") def sameRange(pos: Pos): Boolean

  /** Convert this to a position around `point` that spans a single source line
   */
  @deprecated("removed from the public API", "2.11.0") def toSingleLine: Pos

  /** The content of the line this Position refers to.
   *  @group Common
   */
  @deprecated("removed from the public API", "2.11.0") def lineContent: String

  /** Show a textual representation of the position.
   */
  @deprecated("use `universe.show(position)` instead", "2.11.0") def show: String
}
