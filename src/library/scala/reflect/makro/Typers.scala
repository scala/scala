package scala.reflect
package makro

trait Typers {
  self: Context =>

  import universe._

  /** Contexts that represent macros in-flight, including the current one. Very much like a stack trace, but for macros only.
   *  Can be useful for interoperating with other macros and for imposing compiler-friendly limits on macro expansion.
   *
   *  Is also priceless for emitting sane error messages for macros that are called by other macros on synthetic (i.e. position-less) trees.
   *  In that dire case navigate the ``openMacros'' stack, and it will most likely contain at least one macro with a position-ful macro application.
   *  See ``enclosingPosition'' for a default implementation of this logic.
   *
   *  Unlike `enclosingMacros`, this is a def, which means that it gets recalculated on every invocation,
   *  so it might change depending on what is going on during macro expansion.
   */
  def openMacros: List[Context]

  /** Types along with corresponding trees for which implicit arguments are currently searched.
   *  Can be useful to get information about an application with an implicit parameter that is materialized during current macro expansion.
   *
   *  Unlike `enclosingImplicits`, this is a def, which means that it gets recalculated on every invocation,
   *  so it might change depending on what is going on during macro expansion.
   */
 def openImplicits: List[(Type, Tree)]

  /** Typechecks the provided tree against the expected type ``pt'' in the macro callsite context.
   *
   *  If ``silent'' is false, ``TypeError'' will be thrown in case of a typecheck error.
   *  If ``silent'' is true, the typecheck is silent and will return ``EmptyTree'' if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Ymacro-debug-verbose.
   *  Unlike in ``inferImplicitValue'' and ``inferImplicitView'', ``silent'' is false by default.
   *
   *  Typechecking can be steered with the following optional parameters:
   *    ``withImplicitViewsDisabled'' recursively prohibits implicit views (though, implicit vals will still be looked up and filled in), default value is false
   *    ``withMacrosDisabled'' recursively prohibits macro expansions and macro-based implicits, default value is false
   */
  def typeCheck(tree: Tree, pt: Type = WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree

  /** Infers an implicit value of the expected type ``pt'' in the macro callsite context.
   *  Optional ``pos'' parameter provides a position that will be associated with the implicit search.
   *
   *  If ``silent'' is false, ``TypeError'' will be thrown in case of an inference error.
   *  If ``silent'' is true, the typecheck is silent and will return ``EmptyTree'' if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Xlog-implicits.
   *  Unlike in ``typeCheck'', ``silent'' is true by default.
   */
  def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree

  /** Infers an implicit view from the provided tree ``tree'' from the type ``from'' to the type ``to'' in the macro callsite context.
   *
   *  Optional ``pos'' parameter provides a position that will be associated with the implicit search.
   *  Another optional parameter, ``reportAmbiguous`` controls whether ambiguous implicit errors should be reported.
   *  If we search for a view simply to find out whether one type is coercible to another, it might be desirable to set this flag to ``false''.
   *
   *  If ``silent'' is false, ``TypeError'' will be thrown in case of an inference error.
   *  If ``silent'' is true, the typecheck is silent and will return ``EmptyTree'' if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Xlog-implicits.
   *  Unlike in ``typeCheck'', ``silent'' is true by default.
   */
  def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, reportAmbiguous: Boolean = true, pos: Position = enclosingPosition): Tree

  /** Recursively resets symbols and types in a given tree.
   *
   *  Note that this does not revert the tree to its pre-typer shape.
   *  For more info, read up https://issues.scala-lang.org/browse/SI-5464.
   */
  def resetAllAttrs(tree: Tree): Tree

  /** Recursively resets locally defined symbols and types in a given tree.
   *
   *  Note that this does not revert the tree to its pre-typer shape.
   *  For more info, read up https://issues.scala-lang.org/browse/SI-5464.
   */
  def resetLocalAttrs(tree: Tree): Tree

  /** Represents an error during typechecking
   */
  type TypeError <: Throwable
  val TypeError: TypeErrorExtractor
  abstract class TypeErrorExtractor {
    def unapply(error: TypeError): Option[(Position, String)]
  }
}