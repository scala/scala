package scala.reflect
package makro

abstract class Universe extends scala.reflect.api.Universe {

  val treeBuild: TreeBuilder { val global: Universe.this.type }

  // Symbol extensions ---------------------------------------------------------------

  override type Symbol >: Null <: SymbolContextApi

  /** The extended API of symbols that's supported in macro context universes
   */
  trait SymbolContextApi extends SymbolApi { this: Symbol =>

    // [Eugene++ to Martin] should we also add mutability methods here (similarly to what's done below for trees)?
    // I'm talking about `setAnnotations` and friends

    /** Can this symbol be loaded by a reflective mirror?
     *
     *  Scalac relies on `ScalaSignature' annotation to retain symbols across compilation runs.
     *  Such annotations (also called "pickles") are applied on top-level classes and include information
     *  about all symbols reachable from the annotee. However, local symbols (e.g. classes or definitions local to a block)
     *  are typically unreachable and information about them gets lost.
     *
     *  This method is useful for macro writers who wish to save certain ASTs to be used at runtime.
     *  With `isLocatable' it's possible to check whether a tree can be retained as is, or it needs special treatment.
     */
    def isLocatable: Boolean

    /** Is this symbol static (i.e. with no outer instance)?
     *  Q: When exactly is a sym marked as STATIC?
     *  A: If it's a member of a toplevel object, or of an object contained in a toplevel object, or any number of levels deep.
     *  http://groups.google.com/group/scala-internals/browse_thread/thread/d385bcd60b08faf6
     */
    def isStatic: Boolean
  }

  // Tree extensions ---------------------------------------------------------------

  override type Tree >: Null <: TreeContextApi

  /** The extended API of trees that's supported in macro context universes
   */
  trait TreeContextApi extends TreeApi { this: Tree =>

    /** ... */
    def pos_=(pos: Position): Unit

    /** ... */
    def setPos(newpos: Position): this.type

    /** ... */
    def tpe_=(t: Type): Unit

    /** Set tpe to give `tp` and return this.
     */
    def setType(tp: Type): this.type

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
    def defineType(tp: Type): this.type

    /** ... */
    def symbol_=(sym: Symbol): Unit

    /** ... */
    def setSymbol(sym: Symbol): this.type

    /** ... */
    def attachments: base.Attachments { type Pos = Position }

    /** ... */
    def addAttachment(attachment: Any): this.type

    /** ... */
    def removeAttachment[T: ClassTag]: this.type
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