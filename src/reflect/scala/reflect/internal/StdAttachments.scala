package scala
package reflect
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
    def hasAttachment[T: ClassTag]: Boolean = rawatt.contains[T]

    // cannot be final due to SynchronizedSymbols
    def pos: Position = rawatt.pos
    def pos_=(pos: Position): Unit = rawatt = (rawatt withPos pos)
    def setPos(newpos: Position): this.type = { pos = newpos; this }
  }

  /** Attachment that knows how to import itself into another universe. */
  trait ImportableAttachment {
    def importAttachment(importer: Importer): this.type
  }

  /** Attachment that doesn't contain any reflection artifacts and can be imported as-is. */
  trait PlainAttachment extends ImportableAttachment {
    def importAttachment(importer: Importer): this.type = this
  }

  /** Stores the trees that give rise to a refined type to be used in reification.
   *  Unfortunately typed `CompoundTypeTree` is lacking essential info, and the reifier cannot use `CompoundTypeTree.tpe`.
   *  Therefore we need this hack (see `Reshape.toPreTyperTypeTree` for a detailed explanation).
   */
  case class CompoundTypeTreeOriginalAttachment(parents: List[Tree], stats: List[Tree])

  /** When present, indicates that the host `Ident` has been created from a backquoted identifier.
   */
  case object BackquotedIdentifierAttachment extends PlainAttachment

  /** Identifies trees are either result or intermediate value of for loop desugaring.
   */
  case object ForAttachment extends PlainAttachment

  /** Identifies unit constants which were inserted by the compiler (e.g. gen.mkBlock)
   */
  case object SyntheticUnitAttachment extends PlainAttachment

  /** Untyped list of subpatterns attached to selector dummy. */
  case class SubpatternsAttachment(patterns: List[Tree])

  /** Attached to a class symbol to indicate that its children have been observed
    * via knownDirectSubclasses. Children added subsequently will trigger an
    * error to indicate that the earlier observation was incomplete.
    */
  case object KnownDirectSubclassesCalled extends PlainAttachment

  /** An attachment carrying information between uncurry and erasure */
  case class TypeParamVarargsAttachment(val typeParamRef: Type)
}
