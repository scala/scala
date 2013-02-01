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
    def setAttachments(attachments: scala.reflect.macros.Attachments { type Pos = Position }): this.type = { rawatt = attachments; this }
    def updateAttachment[T: ClassTag](attachment: T): this.type = { rawatt = rawatt.update(attachment); this }
    def removeAttachment[T: ClassTag]: this.type = { rawatt = rawatt.remove[T]; this }

    // cannot be final due to SynchronizedSymbols
    def pos: Position = rawatt.pos
    def pos_=(pos: Position): Unit = rawatt = (rawatt withPos pos)
    def setPos(newpos: Position): this.type = { pos = newpos; this }
  }

  /** When present, indicates that the host `Ident` has been created from a backquoted identifier.
   */
  case object BackquotedIdentifierAttachment

  /** Stores the trees that give rise to a refined type to be used in reification.
   *  Unfortunately typed `CompoundTypeTree` is lacking essential info, and the reifier cannot use `CompoundTypeTree.tpe`.
   *  Therefore we need this hack (see `Reshape.toPreTyperTypeTree` for a detailed explanation).
   */
  case class CompoundTypeTreeOriginalAttachment(parents: List[Tree], stats: List[Tree])
}
