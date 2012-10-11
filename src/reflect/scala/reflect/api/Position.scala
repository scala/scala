package scala.reflect
package api

import scala.reflect.macros.Attachments

/** Position tracks the origin of [[Symbols#Symbol symbols]] and [[Trees#Tree tree nodes]]. They are commonly used when
 *  displaying warnings and errors, to indicate the incorrect point in the program.
 *
 *  A position indicates the [[source source file]] and an [[point offset]]. A position may be
 *  undefined, which means it's pointing to the [[Positions#NoPosition]] element.
 *
 *  <b>Please note that this trait may be refactored in future versions of the Scala reflection API.</b>
 *
 *  @see [[http://docs.scala-lang.org/overviews/reflection/names-exprs-scopes-more.html]]
 *
 *  The compiler adds more information to positions, such a ranges in the source file and defines different types of
 *  positions depending on how a symbol or tree node was generated. The guide fully describes compiler-generated positions.
 *
 *  - INV1: A tree with an offset position never contains a child
 *        with a range position
 *  - INV2: If the child of a tree with a range position also has a range position,
 *        then the child's range is contained in the parent's range.
 *  - INV3: Opaque range positions of children of the same node are non-overlapping
 *        (this means their overlap is at most a single point).
 *
 *  The following tests are useful on positions:
 *  `pos.isDefined`     true if position is not a NoPosition,
 *  `pos.isRange`       true if position is a range,
 *  `pos.isOpaqueRange` true if position is an opaque range,
 *
 *  There are also convenience methods, such as
 *  `pos.startOrPoint`,
 *  `pos.endOrPoint`,
 *  `pos.pointOrElse(default)`.
 *  These are less strict about the kind of position on which they can be applied.
 *
 *  The following conversion methods are often used:
 *  `pos.focus`           converts a range position to an offset position, keeping its point;
 *                        returns all other positions unchanged,
 *  `pos.makeTransparent` converts an opaque range position into a transparent one.
 *                        returns all other positions unchanged.
 *
 *  @groupname Common   Commonly used methods
 */
trait Position extends Attachments {

  /** @inheritdoc */
  type Pos >: Null <: Position

  /** Java file corresponding to the source file of this position.
   *
   *  The return type is `scala.reflect.io.AbstractFile`, which belongs to an experimental part of Scala reflection.
   *  It should not be used unless you know what you are doing. In subsequent releases, this API will be refined
   *  and exposed as a part of scala.reflect.api.
   *
   *  @group Common
   */
  def source: scala.reflect.internal.util.SourceFile

  /** Is this position neither a NoPosition nor a FakePosition?
   *  If isDefined is true, offset and source are both defined.
   *  @group Common
   */
  def isDefined: Boolean

  /** Is this position a range position? */
  def isRange: Boolean

  /** Is this position a transparent position? */
  def isTransparent: Boolean

  /** Is this position a non-transparent range position? */
    def isOpaqueRange: Boolean

  /** If opaque range, make this position transparent. */
  def makeTransparent: Pos

  /** The start of the position's range, error if not a range position. */
  def start: Int

  /** The start of the position's range, or point if not a range position. */
  def startOrPoint: Int

  /** The point (where the ^ is) of the position, which is easiest to access using the [[line]] and [[column]] values.
   *  The [[lineContent line content]] is also available.
   *  @group Common
   */
  def point: Int

  /** The point (where the ^ is) of the position, or else `default` if undefined.
   *  @group Common
   */
  def pointOrElse(default: Int): Int

  /** The end of the position's range, error if not a range position.
   */
  def end: Int

  /** The end of the position's range, or point if not a range position.
   */
  def endOrPoint: Int

  /** The same position with a different start value (if a range).
   */
  def withStart(off: Int): Pos

  /** The same position with a different end value (if a range).
   */
  def withEnd(off: Int): Pos

  /** The same position with a different point value (if a range or offset).
   */
  def withPoint(off: Int): Pos

  /** If this is a range, the union with the other range, with the point of this position.
   *  Otherwise, this position
   */
  def union(pos: Pos): Pos

  /** If this is a range position, the offset position of its point.
   *  Otherwise the position itself
   */
  def focus: Pos

  /** If this is a range position, the offset position of its start.
   *  Otherwise the position itself
   */
  def focusStart: Pos

  /** If this is a range position, the offset position of its end.
   *  Otherwise the position itself
   */
  def focusEnd: Pos

  /** Does this position include the given position `pos`?
   *  This holds if `this` is a range position and its range [start..end]
   *  is the same or covers the range of the given position, which may or may not be a range position.
   */
  def includes(pos: Pos): Boolean

  /** Does this position properly include the given position `pos` ("properly" meaning their
   *  ranges are not the same)?
   */
  def properlyIncludes(pos: Pos): Boolean

  /** Does this position precede that position?
   *  This holds if both positions are defined and the end point of this position
   *  is not larger than the start point of the given position.
   */
  def precedes(pos: Pos): Boolean

  /** Does this position properly precede the given position `pos` ("properly" meaning their ranges
   *  do not share a common point).
   */
  def properlyPrecedes(pos: Pos): Boolean

  /** Does this position overlap with that position?
   *  This holds if both positions are ranges and there is an interval of
   *  non-zero length that is shared by both position ranges.
   */
  def overlaps(pos: Pos): Boolean

  /** Does this position cover the same range as that position?
   *  Holds only if both position are ranges
   */
  def sameRange(pos: Pos): Boolean

  /** The position indicates a [[column `column`]] and the `line` in the source file.
   *  @group Common
   */
  def line: Int

  /** The position indicates a `column` and the [[line `line`]] in the source file.
   *  @group Common
   */
  def column: Int

  /** Convert this to a position around `point` that spans a single source line
   */
  def toSingleLine: Pos

  /** The content of the line this Position refers to.
   *  @group Common
   */
  def lineContent: String

  /** Show a textual representation of the position.
   */
  def show: String
}
