package scala.reflect
package macros

abstract class Universe extends scala.reflect.api.Universe {

  val treeBuild: TreeBuilder { val global: Universe.this.type }

  trait AttachableApi {
    /** ... */
    def attachments: Attachments { type Pos = Position }

    /** ... */
    def updateAttachment[T: ClassTag](attachment: T): AttachableApi.this.type

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

    def setAnnotations(annots: Annotation*): Symbol

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

  /** Mark a variable as captured; i.e. force boxing in a *Ref type.
   */
  def captureVariable(vble: Symbol): Unit

  /** Mark given identifier as a reference to a captured variable itself
   *  suppressing dereferencing with the `elem` field.
   */
  def referenceCapturedVariable(vble: Symbol): Tree

  /** Convert type of a captured variable to *Ref type.
   */
  def capturedVariableType(vble: Symbol): Type

  type Run <: RunContextApi

  /** Compilation run uniquely identifies current invocation of the compiler
   *  (e.g. can be used to implement per-run caches for macros) and provides access to units of work
   *  of the invocation (currently processed unit of work and the list of all units).
   */
  trait RunContextApi {
    /** Currently processed unit of work (a real or a virtual file). */
    def currentUnit: CompilationUnit

    /** All units of work comprising this compilation run. */
    def units: Iterator[CompilationUnit]
  }

  type CompilationUnit <: CompilationUnitContextApi

  /** Compilation unit describes a unit of work of the compilation run.
   *  It provides such information as file name, textual representation of the unit and the underlying AST.
   */
  trait CompilationUnitContextApi {
    /** Source file corresponding to this compilation unit.
     *
     *  Exposes information about the file as a part of a real or virtual file system
     *  along with the contents of that file.
     *
     *  The return type is [[scala.reflect.io.AbstractFile]], which belongs to an experimental part of Scala reflection.
     *  It should not be used unless you know what you are doing. In subsequent releases, this API will be refined
     *  and exposed as a part of scala.reflect.api.
     */
    def source: scala.reflect.internal.util.SourceFile

    /** The AST that corresponds to this compilation unit. */
    def body: Tree
  }
}