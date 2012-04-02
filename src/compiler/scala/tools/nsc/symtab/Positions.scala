package scala.tools.nsc
package symtab

import scala.tools.nsc.util.{ SourceFile, Position, OffsetPosition, NoPosition }

trait Positions extends scala.reflect.internal.Positions {
self: scala.tools.nsc.symtab.SymbolTable =>

  def rangePos(source: SourceFile, start: Int, point: Int, end: Int) =
    new OffsetPosition(source, point)

  def validatePositions(tree: Tree) {}

  type Position = scala.tools.nsc.util.Position
  val NoPosition = scala.tools.nsc.util.NoPosition

  type TreeAnnotation = scala.tools.nsc.util.TreeAnnotation
  val NoTreeAnnotation: TreeAnnotation                      = NoPosition
  def positionToAnnotation(pos: Position): TreeAnnotation   = pos
  def annotationToPosition(annot: TreeAnnotation): Position = annot.pos
  override def _checkSetAnnotation(tree: Tree, annot: TreeAnnotation): Unit = {
    if (tree.pos != NoPosition && tree.pos != annot.pos) debugwarn("Overwriting annotation "+ tree.annotation +" of tree "+ tree +" with annotation "+ annot)
    // if ((tree.annotation.isInstanceOf[scala.tools.nsc.util.Position] || !annot.isInstanceOf[scala.tools.nsc.util.Position]) && tree.isInstanceOf[Block])
    //   println("Updating block from "+ tree.annotation +" to "+ annot)
  }
  def focusPos(pos: Position): Position = pos.focus
  def isRangePos(pos: Position): Boolean = pos.isRange
  def showPos(pos: Position): String = pos.show

}
