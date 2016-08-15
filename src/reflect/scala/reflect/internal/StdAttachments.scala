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

  /** Attached to a Function node during type checking when the expected type is a SAM type (and not a built-in FunctionN).
    *
    * Ideally, we'd move to Dotty's Closure AST, which tracks the environment,
    * the lifted method that has the implementation, and the target type.
    * For backwards compatibility, an attachment is the best we can do right now.
    *
    * @param samTp the expected type that triggered sam conversion (may be a subtype of the type corresponding to sam's owner)
    * @param sam the single abstract method implemented by the Function we're attaching this to
    *
    * @since 2.12.0-M4
    */
  case class SAMFunction(samTp: Type, sam: Symbol) extends PlainAttachment

  case object DelambdafyTarget extends PlainAttachment

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

  abstract class InlineAnnotatedAttachment
  case object NoInlineCallsiteAttachment extends InlineAnnotatedAttachment
  case object InlineCallsiteAttachment extends InlineAnnotatedAttachment

  /** Attached to a local class that has its outer field elided. A `null` constant may be passed
    * in place of the outer parameter, can help callers to avoid capturing the outer instance.
    */
  case object OuterArgCanBeElided extends PlainAttachment

  case object UseInvokeSpecial extends PlainAttachment
}
