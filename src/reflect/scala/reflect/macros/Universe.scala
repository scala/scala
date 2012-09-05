package scala.reflect
package macros

abstract class Universe extends scala.reflect.api.Universe {

  val treeBuild: TreeBuilder { val global: Universe.this.type }

  trait AttachableApi {
    /** ... */
    def attachments: base.Attachments { type Pos = Position }

    /** ... */
    def addAttachment(attachment: Any): AttachableApi.this.type

    /** ... */
    def removeAttachment[T: ClassTag]: AttachableApi.this.type
  }

  // Symbol extensions ---------------------------------------------------------------

  override type Symbol >: Null <: SymbolContextApi

  /** The extended API of symbols that's supported in macro context universes
   */
  trait SymbolContextApi extends SymbolApi with AttachableApi { self: Symbol =>

    def deSkolemize: Symbol

    /** The position of this symbol
     */
    def pos: Position

    def setTypeSignature(tpe: Type): Symbol

    def setAnnotations(annots: AnnotationInfo*): Symbol

    def setName(name: Name): Symbol

    def setPrivateWithin(sym: Symbol): Symbol
  }

  // Tree extensions ---------------------------------------------------------------

  override type Tree >: Null <: TreeContextApi

  /** The extended API of trees that's supported in macro context universes
   */
  trait TreeContextApi extends TreeApi with AttachableApi { self: Tree =>

    /** ... */
    def pos_=(pos: Position): Unit

    /** ... */
    def setPos(newpos: Position): Tree

    /** ... */
    def tpe_=(t: Type): Unit

    /** Set tpe to give `tp` and return this.
     */
    def setType(tp: Type): Tree

    /** Like `setType`, but if this is a previously empty TypeTree that
     *  fact is remembered so that resetAllAttrs will snap back.
     *
     *  @PP: Attempting to elaborate on the above, I find: If defineType
     *  is called on a TypeTree whose type field is null or NoType,
     *  this is recorded as "wasEmpty = true". That value is used in
     *  ResetAttrsTraverser, which nulls out the type field of TypeTrees
     *  for which wasEmpty is true, leaving the others alone.
     *
     *  resetAllAttrs is used in situations where some speculative
     *  typing of a tree takes place, fails, and the tree needs to be
     *  returned to its former state to try again. So according to me:
     *  using `defineType` instead of `setType` is how you communicate
     *  that the type being set does not depend on any previous state,
     *  and therefore should be abandoned if the current line of type
     *  inquiry doesn't work out.
     */
    def defineType(tp: Type): Tree

    /** ... */
    def symbol_=(sym: Symbol): Unit

    /** ... */
    def setSymbol(sym: Symbol): Tree
  }

  override type SymTree >: Null <: Tree with SymTreeContextApi

  /** The extended API of sym trees that's supported in macro context universes
   */
  trait SymTreeContextApi extends SymTreeApi { this: SymTree =>
    var symbol: Symbol
  }

  override type TypeTree >: Null <: TypTree with TypeTreeContextApi

  /** The extended API of sym trees that's supported in macro context universes
   */
  trait TypeTreeContextApi extends TypeTreeApi { this: TypeTree =>
    def setOriginal(tree: Tree): this.type
  }

  override type Ident >: Null <: RefTree with IdentContextApi

  /** The extended API of idents that's supported in macro context universes
   */
  trait IdentContextApi extends IdentApi { this: Ident =>
    def isBackquoted: Boolean
  }
}