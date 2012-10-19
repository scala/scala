package scala.reflect
package internal

trait StdAttachments {
  self: SymbolTable =>

  /**
   * Common code between reflect-internal Symbol and Tree related to Attachments.
   */
  trait Attachable {
    protected var rawatt: scala.reflect.macros.Attachments { type Pos = Position } = NoPosition
    def attachments = rawatt
    def updateAttachment[T: ClassTag](attachment: T): this.type = { rawatt = rawatt.update(attachment); this }
    def removeAttachment[T: ClassTag]: this.type = { rawatt = rawatt.remove[T]; this }

    // cannot be final due to SynchronizedSymbols
    def pos: Position = rawatt.pos
    def pos_=(pos: Position): Unit = rawatt = (rawatt withPos pos)
    def setPos(newpos: Position): this.type = { pos = newpos; this }
  }

  case object BackquotedIdentifierAttachment

  case class CompoundTypeTreeOriginalAttachment(parents: List[Tree], stats: List[Tree])

  case class MacroExpansionAttachment(original: Tree)
}
