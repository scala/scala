package scala.tools.nsc.interactive

import scala.tools.nsc.util.{SourceFile, Position, NoPosition}

trait RichCompilationUnits { self: Global =>

  class RichCompilationUnit(source: SourceFile) extends CompilationUnit(source) {

    /** The runid of the latest compiler run that typechecked this unit,
     *  or else @see NotLoaded, JustParsed
     */
    var status: Int = NotLoaded

    /** the current edit point offset */
    var editPoint: Int = -1

    /** The position of a targeted type check
     *  If this is different from NoPosition, the type checking
     *  will stop once a tree that contains this position range
     *  is fully attributed.
     */
    var _targetPos: Position = NoPosition
    override def targetPos: Position = _targetPos
    def targetPos_=(p: Position) { _targetPos = p }

    var contexts: Contexts = new Contexts

  }
}
