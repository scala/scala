package scala.reflect
package api

trait Positions { self: Universe =>
  /** TreeAnnotation is a generalisation of Position.
   *
   * TreeAnnotation cannot be an upperbound of Position since the corresponding classes
   * must live outside of the universe for backwards compatibility (see scala.tools.nsc.util.Position).
   * Thus, represent subtyping as coercions.
   *
   * Typically, positionToAnnotation is the identity, and annotationToPosition returns annot.pos
   */
  type TreeAnnotation // <: { def pos: Position }
  val NoTreeAnnotation: TreeAnnotation
  implicit def positionToAnnotation(pos: Position): TreeAnnotation  // = pos
  def annotationToPosition(annot: TreeAnnotation): Position         // = annot.pos
  def _checkSetAnnotation(tree: Tree, annot: TreeAnnotation): Unit = () // check that annot may overwrite tree.annot

  type Position // <: TreeAnnotation, but not practical to enforce this (would require moving Position, SourceFile, Reporter,... into the universe)
  val NoPosition: Position
}