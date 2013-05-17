package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  The refinement of [[scala.reflect.api.Universe]] for the use by macro writers.
 *
 *  This universe provides mutability for reflection artifacts (e.g. macros can change types of compiler trees,
 *  add annotation to symbols representing definitions, etc) and exposes some internal compiler functionality
 *  such as `Symbol.deSkolemize` or `Tree.attachments`.
 *  @groupname Macros Macro Specific Additions
 *  @groupprio Macros -1
 *
 *  @contentDiagram hideNodes "*Api"
 */
abstract class Universe extends scala.reflect.api.Universe {

  /** A factory that encapsulates common tree-building functions.
   *  @group Macros
   */
  val treeBuild: TreeBuilder { val global: Universe.this.type }

  /** The API of reflection artifacts that support [[scala.reflect.macros.Attachments]].
   *  These artifacts are trees and symbols.
   *  @group Macros
   */
  trait AttachableApi {
    /** The attachment of the reflection artifact. */
    def attachments: Attachments { type Pos = Position }

    /** Updates the attachment with the payload slot of T added/updated with the provided value.
     *  Replaces an existing payload of the same type, if exists.
     *  Returns the reflection artifact itself.
     */
    def updateAttachment[T: ClassTag](attachment: T): AttachableApi.this.type

    /** Update the attachment with the payload of the given class type `T` removed.
     *  Returns the reflection artifact itself.
     */
    def removeAttachment[T: ClassTag]: AttachableApi.this.type
  }

  // Symbol extensions ---------------------------------------------------------------

  /**  The `Symbol` API is extended for macros: See [[SymbolContextApi]] for details.
   *
   *  @group Macros
   */
  override type Symbol >: Null <: SymbolContextApi

  /** The extended API of symbols that's supported in macro context universes
   *  @group API
   */
  trait SymbolContextApi extends SymbolApi with AttachableApi { self: Symbol =>

    /** If this symbol is a skolem, its corresponding type parameter, otherwise the symbol itself.
     *
     *  [[https://groups.google.com/forum/#!msg/scala-internals/0j8laVNTQsI/kRXMF_c8bGsJ To quote Martin Odersky]],
     *  skolems are synthetic type "constants" that are copies of existentially bound or universally
     *  bound type variables. E.g. if one is inside the right-hand side of a method:
     *
     *  {{{
     *  def foo[T](x: T) = ... foo[List[T]]....
     *  }}}
     *
     *  the skolem named `T` refers to the unknown type instance of `T` when `foo` is called. It needs to be different
     *  from the type parameter because in a recursive call as in the `foo[List[T]]` above the type parameter gets
     *  substituted with `List[T]`, but the ''type skolem'' stays what it is.
     *
     *  The other form of skolem is an ''existential skolem''. Say one has a function
     *
     *  {{{
     *  def bar(xs: List[T] forSome { type T }) = xs.head
     *  }}}
     *
     *  then each occurrence of `xs` on the right will have type `List[T']` where `T'` is a fresh copy of `T`.
     */
    def deSkolemize: Symbol

    /** The position of this symbol. */
    def pos: Position

    /** Sets the `typeSignature` of the symbol. */
    def setTypeSignature(tpe: Type): Symbol

    /** Sets the `annotations` of the symbol. */
    def setAnnotations(annots: Annotation*): Symbol

    /** Sets the `name` of the symbol. */
    def setName(name: Name): Symbol

    /** Sets the `privateWithin` of the symbol. */
    def setPrivateWithin(sym: Symbol): Symbol
  }

  // Tree extensions ---------------------------------------------------------------

  /**  The `Tree` API is extended for macros: See [[TreeContextApi]] for details.
   *
   *  @group Macros
   */
  override type Tree >: Null <: TreeContextApi

  /** The extended API of trees that's supported in macro context universes
   *  @group API
   */
  trait TreeContextApi extends TreeApi with AttachableApi { self: Tree =>

    /** Sets the `pos` of the tree. Returns `Unit`. */
    def pos_=(pos: Position): Unit

    /** Sets the `pos` of the tree. Returns the tree itself. */
    def setPos(newpos: Position): Tree

    /** Sets the `tpe` of the tree. Returns `Unit`. */
    @deprecated("Use setType", "2.11.0") def tpe_=(t: Type): Unit

    /** Sets the `tpe` of the tree. Returns the tree itself. */
    def setType(tp: Type): Tree

    /** Like `setType`, but if this is a previously empty TypeTree that
     *  fact is remembered so that resetAllAttrs will snap back.
     *
     *  \@PP: Attempting to elaborate on the above, I find: If defineType
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

    /** Sets the `symbol` of the tree. Returns `Unit`. */
    def symbol_=(sym: Symbol): Unit

    /** Sets the `symbol` of the tree. Returns the tree itself. */
    def setSymbol(sym: Symbol): Tree
  }

  /** @inheritdoc */
  override type SymTree >: Null <: Tree with SymTreeContextApi

  /** The extended API of sym trees that's supported in macro context universes
   *  @group API
   */
  trait SymTreeContextApi extends SymTreeApi { this: SymTree =>
    /** Sets the `symbol` field of the sym tree. */
    var symbol: Symbol
  }

  /** @inheritdoc */
  override type TypeTree >: Null <: TypTree with TypeTreeContextApi

  /** The extended API of sym trees that's supported in macro context universes
   *  @group API
   */
  trait TypeTreeContextApi extends TypeTreeApi { this: TypeTree =>
    /** Sets the `original` field of the type tree. */
    def setOriginal(tree: Tree): this.type
  }

  /** @inheritdoc */
  override type Ident >: Null <: RefTree with IdentContextApi

  /** The extended API of idents that's supported in macro context universes
   *  @group API
   */
  trait IdentContextApi extends IdentApi { this: Ident =>
    /** Was this ident created from a backquoted identifier? */
    def isBackquoted: Boolean
  }

  /** Mark a variable as captured; i.e. force boxing in a *Ref type.
   *  @group Macros
   */
  def captureVariable(vble: Symbol): Unit

  /** Mark given identifier as a reference to a captured variable itself
   *  suppressing dereferencing with the `elem` field.
   *  @group Macros
   */
  def referenceCapturedVariable(vble: Symbol): Tree

  /** Convert type of a captured variable to *Ref type.
   *  @group Macros
   */
  def capturedVariableType(vble: Symbol): Type

  /** The type of compilation runs.
   *  @template
   *  @group Macros
   */
  type Run <: RunContextApi

  /** Compilation run uniquely identifies current invocation of the compiler
   *  (e.g. can be used to implement per-run caches for macros) and provides access to units of work
   *  of the invocation (currently processed unit of work and the list of all units).
   *  @group API
   */
  trait RunContextApi {
    /** Currently processed unit of work (a real or a virtual file). */
    def currentUnit: CompilationUnit

    /** All units of work comprising this compilation run. */
    def units: Iterator[CompilationUnit]
  }

  /** The type of compilation units.
   *  @template
   *  @group Macros
   */
  type CompilationUnit <: CompilationUnitContextApi

  /** Compilation unit describes a unit of work of the compilation run.
   *  It provides such information as file name, textual representation of the unit and the underlying AST.
   *  @group API
   */
  trait CompilationUnitContextApi {
    /** Source file corresponding to this compilation unit.
     *
     *  Exposes information about the file as a part of a real or virtual file system
     *  along with the contents of that file.
     *
     *  The return type is `scala.reflect.io.AbstractFile`, which belongs to an experimental part of Scala reflection.
     *  It should not be used unless you know what you are doing. In subsequent releases, this API will be refined
     *  and exposed as a part of scala.reflect.api.
     */
    def source: scala.reflect.internal.util.SourceFile

    /** The AST that corresponds to this compilation unit. */
    def body: Tree
  }
}
