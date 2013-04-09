package scala.reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.Context the Scala macros context]] that exposes
 *  enclosing trees (method, class, compilation unit and currently compiled application),
 *  the enclosing position of the macro expansion, as well as macros and implicits
 *  that are currently in-flight.
 */
trait Enclosures {
  self: Context =>

  /** The tree that undergoes macro expansion.
   *  Can be useful to get an offset or a range position of the entire tree being processed.
   */
  def macroApplication: Tree

  /** The semantic role that `macroApplication` plays in the code.
   */
  type MacroRole

  /** The role that represents an application of a term macro,
   *  e.g. `M(2)(3)` in `val x = M(2)(3)` or `M(a, b)` in `x match { case x @ M(a, b) => }`.
   */
  def APPLY_ROLE: MacroRole

  /** The semantic role that `macroApplication` plays in the code.
   */
  def macroRole: MacroRole

  /** Contexts that represent macros in-flight, including the current one. Very much like a stack trace, but for macros only.
   *  Can be useful for interoperating with other macros and for imposing compiler-friendly limits on macro expansion.
   *
   *  Is also priceless for emitting sane error messages for macros that are called by other macros on synthetic (i.e. position-less) trees.
   *  In that dire case navigate the `enclosingMacros` stack, and it will most likely contain at least one macro with a position-ful macro application.
   *  See `enclosingPosition` for a default implementation of this logic.
   *
   *  Unlike `openMacros`, this is a val, which means that it gets initialized when the context is created
   *  and always stays the same regardless of whatever happens during macro expansion.
   */
  def enclosingMacros: List[Context]

  /** Types along with corresponding trees for which implicit arguments are currently searched.
   *  Can be useful to get information about an application with an implicit parameter that is materialized during current macro expansion.
   *
   *  Unlike `openImplicits`, this is a val, which means that it gets initialized when the context is created
   *  and always stays the same regardless of whatever happens during macro expansion.
   */
  def enclosingImplicits: List[(Type, Tree)]

  /** Tries to guess a position for the enclosing application.
   *  But that is simple, right? Just dereference `pos` of `macroApplication`? Not really.
   *  If we're in a synthetic macro expansion (no positions), we must do our best to infer the position of something that triggerd this expansion.
   *  Surprisingly, quite often we can do this by navigation the `enclosingMacros` stack.
   */
  def enclosingPosition: Position

  /** Tree that corresponds to the enclosing method, or EmptyTree if not applicable.
   */
  @deprecated("Use enclosingDef instead, but be wary of changes in semantics", "2.10.1")
  def enclosingMethod: Tree

  /** Tree that corresponds to the enclosing class, or EmptyTree if not applicable.
   */
  @deprecated("Use enclosingImpl instead, but be wary of changes in semantics", "2.10.1")
  def enclosingClass: Tree

  /** Tree that corresponds to the enclosing DefDef tree.
   *  Throws `EnclosureException` if there's no such enclosing tree.
   */
  def enclosingDef: universe.DefDef

  /** Tree that corresponds to the enclosing Template tree.
   *  Throws `EnclosureException` if there's no such enclosing tree.
   */
  def enclosingTemplate: universe.Template

  /** Tree that corresponds to the enclosing ImplDef tree (i.e. either ClassDef or ModuleDef).
   *  Throws `EnclosureException` if there's no such enclosing tree.
   */
  def enclosingImpl: universe.ImplDef

  /** Tree that corresponds to the enclosing PackageDef tree.
   *  Throws `EnclosureException` if there's no such enclosing tree.
   */
  def enclosingPackage: universe.PackageDef

  /** Compilation unit that contains this macro application.
   */
  def enclosingUnit: CompilationUnit

  /** Compilation run that contains this macro application.
   */
  def enclosingRun: Run

  /** Indicates than one of the enclosure methods failed to find a tree
   *  of required type among enclosing trees.
   */
  case class EnclosureException(expected: Class[_], enclosingTrees: List[Tree])
  extends Exception(s"Couldn't find a tree of type $expected among enclosing trees $enclosingTrees")
}