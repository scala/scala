package scala.tools.nsc
package ast

import scala.reflect.internal.util.{ SourceFile, Position, OffsetPosition, NoPosition }

trait Positions extends scala.reflect.internal.Positions {
  self: Global =>

  def rangePos(source: SourceFile, start: Int, point: Int, end: Int) =
    new OffsetPosition(source, point)

  def validatePositions(tree: Tree) {}

  class ValidatingPosAssigner extends PosAssigner {
    var pos: Position = _
    override def traverse(t: Tree) {
      if (t eq EmptyTree) ()
      else if (t.pos == NoPosition) super.traverse(t setPos pos)
      else if (globalPhase.id <= currentRun.picklerPhase.id) {
        // When we prune due to encountering a position, traverse the
        // pruned children so we can warn about those lacking positions.
        t.children foreach { c =>
          if ((c eq EmptyTree) || (c eq emptyValDef)) ()
          else if (c.pos == NoPosition) {
            reporter.warning(t.pos, " Positioned tree has unpositioned child in phase " + globalPhase)
            inform("parent: " + treeSymStatus(t))
            inform(" child: " + treeSymStatus(c) + "\n")
          }
        }
      }
    }
  }

  override protected[this] lazy val posAssigner: PosAssigner =
    if (settings.Yrangepos.value && settings.debug.value || settings.Yposdebug.value) new ValidatingPosAssigner
    else new DefaultPosAssigner
}
