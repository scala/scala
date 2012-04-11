package scala.reflect
package api

trait Positions {
  self: Universe =>

  // [Eugene] in quite a lot of situations (mostly related to error reporting) we need positions in the API
  // however it seems that neither runtime compilation, nor macros need facilities to create positions from scratch
  // both emit ASTs, which can be automatically transformed into synthetic sources and assigned with synthetic positions
  // hence I added possibilities to inspect everything we can, but add any position factories
  // this simplified a lot of things, the biggest of them is that we don't need to expose SourceFile/AbstractFile
  type Position <: scala.reflect.api.Position
  val NoPosition: Position

  /** A position that wraps a set of trees.
   *  The point of the wrapping position is the point of the default position.
   *  If some of the trees are ranges, returns a range position enclosing all ranges
   *  Otherwise returns default position.
   */
  def wrappingPos(default: Position, trees: List[Tree]): Position

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If all some the trees are non-synthetic, returns a range position enclosing the non-synthetic trees
   *  Otherwise returns a synthetic offset position to point.
   */
  def wrappingPos(trees: List[Tree]): Position

  /** Ensure that given tree has no positions that overlap with
   *  any of the positions of `others`. This is done by
   *  shortening the range or assigning TransparentPositions
   *  to some of the nodes in `tree`.
   */
  def ensureNonOverlapping(tree: Tree, others: List[Tree])

  /** Assigns a given position to all position-less nodes of a given AST.
   */
  def atPos[T <: Tree](pos: Position)(tree: T): T
}

/** The Position class and its subclasses represent positions of ASTs and symbols.
 *  Except for NoPosition and FakePos, every position refers to a SourceFile
 *  and to an offset in the sourcefile (its `point`). For batch compilation,
 *  that's all. For interactive IDE's there are also RangePositions
 *  and TransparentPositions. A RangePosition indicates a start and an end
 *  in addition to its point. TransparentPositions are a subclass of RangePositions.
 *  Range positions that are not transparent are called opaque.
 *  Trees with RangePositions need to satisfy the following invariants.
 *
 *  INV1: A tree with an offset position never contains a child
 *        with a range position
 *  INV2: If the child of a tree with a range position also has a range position,
 *        then the child's range is contained in the parent's range.
 *  INV3: Opaque range positions of children of the same node are non-overlapping
 *        (this means their overlap is at most a single point).
 *
 *  The following tests are useful on positions:
 *
 *  pos.isDefined     true if position is not a NoPosition nor a FakePosition
 *  pos.isRange       true if position is a range
 *  pos.isOpaqueRange true if position is an opaque range
 *
 *  The following accessor methods are provided:
 *
 *  pos.source        The source file of the position, which must be defined
 *  pos.point         The offset of the position's point, which must be defined
 *  pos.start         The start of the position, which must be a range
 *  pos.end           The end of the position, which must be a range
 *
 *  There are also convenience methods, such as
 *
 *  pos.startOrPoint
 *  pos.endOrPoint
 *  pos.pointOrElse(default)
 *
 *  These are less strict about the kind of position on which they can be applied.
 *
 *  The following conversion methods are often used:
 *
 *  pos.focus           converts a range position to an offset position, keeping its point;
 *                      returns all other positions unchanged.
 *  pos.makeTransparent converts an opaque range position into a transparent one.
 *                      returns all other positions unchanged.
 */
trait Position extends Attachment {

  /** Java file corresponding to the source file of this position.
   */
  def fileInfo: java.io.File

  /** Content of the source file that contains this position.
   */
  def fileContent: Array[Char]

  /** Is this position neither a NoPosition nor a FakePosition?
   *  If isDefined is true, offset and source are both defined.
   */
  def isDefined: Boolean

  /** Is this position a transparent position? */
  def isTransparent: Boolean

  /** Is this position a range position? */
  def isRange: Boolean

  /** Is this position a non-transparent range position? */
  def isOpaqueRange: Boolean

  /** if opaque range, make this position transparent */
  def makeTransparent: Position

  /** The start of the position's range, error if not a range position */
  def start: Int

  /** The start of the position's range, or point if not a range position */
  def startOrPoint: Int

  /**  The point (where the ^ is) of the position */
  def point: Int

  /**  The point (where the ^ is) of the position, or else `default` if undefined */
  def pointOrElse(default: Int): Int

  /** The end of the position's range, error if not a range position */
  def end: Int

  /** The end of the position's range, or point if not a range position */
  def endOrPoint: Int

  /** The same position with a different start value (if a range) */
  def withStart(off: Int): Position

  /** The same position with a different end value (if a range) */
  def withEnd(off: Int): Position

  /** The same position with a different point value (if a range or offset) */
  def withPoint(off: Int): Position

  /** If this is a range, the union with the other range, with the point of this position.
   *  Otherwise, this position
   */
  def union(pos: Position): Position

  /** If this is a range position, the offset position of its start.
   *  Otherwise the position itself
   */
  def focusStart: Position

  /** If this is a range position, the offset position of its point.
   *  Otherwise the position itself
   */
  def focus: Position

  /** If this is a range position, the offset position of its end.
   *  Otherwise the position itself
   */
  def focusEnd: Position

  /** Does this position include the given position `pos`.
   *  This holds if `this` is a range position and its range [start..end]
   *  is the same or covers the range of the given position, which may or may not be a range position.
   */
  def includes(pos: Position): Boolean

  /** Does this position properly include the given position `pos` ("properly" meaning their
   *  ranges are not the same)?
   */
  def properlyIncludes(pos: Position): Boolean

  /** Does this position precede that position?
   *  This holds if both positions are defined and the end point of this position
   *  is not larger than the start point of the given position.
   */
  def precedes(pos: Position): Boolean

  /** Does this position properly precede the given position `pos` ("properly" meaning their ranges
   *  do not share a common point).
   */
  def properlyPrecedes(pos: Position): Boolean

  /** Does this position overlap with that position?
   *  This holds if both positions are ranges and there is an interval of
   *  non-zero length that is shared by both position ranges.
   */
  def overlaps(pos: Position): Boolean

  /** Does this position cover the same range as that position?
   *  Holds only if both position are ranges
   */
  def sameRange(pos: Position): Boolean

  def line: Int

  def column: Int

  /** Convert this to a position around `point` that spans a single source line */
  def toSingleLine: Position

  def lineContent: String

  def show: String
}
