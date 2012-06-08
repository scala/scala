package scala.reflect
package makro

trait Enclosures {
  self: Context =>

  /** The tree that undergoes macro expansion.
   *  Can be useful to get an offset or a range position of the entire tree being processed.
   */
  val macroApplication: Tree

  /** Contexts that represent macros in-flight, including the current one. Very much like a stack trace, but for macros only.
   *  Can be useful for interoperating with other macros and for imposing compiler-friendly limits on macro expansion.
   *
   *  Is also priceless for emitting sane error messages for macros that are called by other macros on synthetic (i.e. position-less) trees.
   *  In that dire case navigate the ``enclosingMacros'' stack, and it will most likely contain at least one macro with a position-ful macro application.
   *  See ``enclosingPosition'' for a default implementation of this logic.
   *
   *  Unlike `openMacros`, this is a val, which means that it gets initialized when the context is created
   *  and always stays the same regardless of whatever happens during macro expansion.
   */
  val enclosingMacros: List[Context]

  /** Types along with corresponding trees for which implicit arguments are currently searched.
   *  Can be useful to get information about an application with an implicit parameter that is materialized during current macro expansion.
   *
   *  Unlike `openImplicits`, this is a val, which means that it gets initialized when the context is created
   *  and always stays the same regardless of whatever happens during macro expansion.
   */
  val enclosingImplicits: List[(Type, Tree)]

  /** Tries to guess a position for the enclosing application.
   *  But that is simple, right? Just dereference ``pos'' of ``macroApplication''? Not really.
   *  If we're in a synthetic macro expansion (no positions), we must do our best to infer the position of something that triggerd this expansion.
   *  Surprisingly, quite often we can do this by navigation the ``enclosingMacros'' stack.
   */
  val enclosingPosition: Position

  /** Tree that corresponds to the enclosing application, or EmptyTree if not applicable.
   */
  val enclosingApplication: Tree

  /** Tree that corresponds to the enclosing method, or EmptyTree if not applicable.
   */
  val enclosingMethod: Tree

  /** Tree that corresponds to the enclosing class, or EmptyTree if not applicable.
   */
  val enclosingClass: Tree

  /** Compilation unit that contains this macro application.
   */
  val enclosingUnit: CompilationUnit
}