package scala.reflect.reify
package codegen

trait GenPositions {
  self: Reifier =>

  import global._

  // we do not reify positions because this inflates resulting trees, but doesn't buy as anything
  // where would one use positions? right, in error messages
  // but I can hardly imagine when one would need a position that points to the reified code
  // usually reified trees are used to compose macro expansions or to be fed to the runtime compiler
  // however both macros and toolboxes have their own means to report errors in synthetic trees
  def reifyPosition(pos: Position): Tree =
    reifyMirrorObject(NoPosition)
}
