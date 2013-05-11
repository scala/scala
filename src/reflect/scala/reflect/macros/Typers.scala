package scala.reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.Context the Scala macros context]] that
 *  partially exposes the type checker to macro writers.
 */
trait Typers {
  self: Context =>

  import universe._

  /** Contexts that represent macros in-flight, including the current one. Very much like a stack trace, but for macros only.
   *  Can be useful for interoperating with other macros and for imposing compiler-friendly limits on macro expansion.
   *
   *  Is also priceless for emitting sane error messages for macros that are called by other macros on synthetic (i.e. position-less) trees.
   *  In that dire case navigate the `openMacros` stack, and it will most likely contain at least one macro with a position-ful macro application.
   *  See `enclosingPosition` for a default implementation of this logic.
   *
   *  Unlike `enclosingMacros`, this is a def, which means that it gets recalculated on every invocation,
   *  so it might change depending on what is going on during macro expansion.
   */
  def openMacros: List[Context]

  /** Information about one of the currently considered implicit candidates.
   *  Candidates are used in plural form, because implicit parameters may themselves have implicit parameters,
   *  hence implicit searches can recursively trigger other implicit searches.
   *
   *  Can be useful to get information about an application with an implicit parameter that is materialized during current macro expansion.
   *  If we're in an implicit macro being expanded, it's included in this list.
   *
   *  Unlike `enclosingImplicits`, this is a def, which means that it gets recalculated on every invocation,
   *  so it might change depending on what is going on during macro expansion.
   */
  def openImplicits: List[(Type, Tree)]

  /** Typechecks the provided tree against the expected type `pt` in the macro callsite context.
   *
   *  If `silent` is false, `TypecheckException` will be thrown in case of a typecheck error.
   *  If `silent` is true, the typecheck is silent and will return `EmptyTree` if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Ymacro-debug-verbose.
   *  Unlike in `inferImplicitValue` and `inferImplicitView`, `silent` is false by default.
   *
   *  Typechecking can be steered with the following optional parameters:
   *    `withImplicitViewsDisabled` recursively prohibits implicit views (though, implicit vals will still be looked up and filled in), default value is false
   *    `withMacrosDisabled` recursively prohibits macro expansions and macro-based implicits, default value is false
   *
   *  @throws [[scala.reflect.macros.TypecheckException]]
   */
  def typeCheck(tree: Tree, pt: Type = WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree

  /** Infers an implicit value of the expected type `pt` in the macro callsite context.
   *  Optional `pos` parameter provides a position that will be associated with the implicit search.
   *
   *  If `silent` is false, `TypecheckException` will be thrown in case of an inference error.
   *  If `silent` is true, the typecheck is silent and will return `EmptyTree` if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Xlog-implicits.
   *  Unlike in `typeCheck`, `silent` is true by default.
   *
   *  @throws [[scala.reflect.macros.TypecheckException]]
   */
  def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree

  /** Infers an implicit view from the provided tree `tree` of the type `from` to the type `to` in the macro callsite context.
   *  Optional `pos` parameter provides a position that will be associated with the implicit search.
   *
   *  If `silent` is false, `TypecheckException` will be thrown in case of an inference error.
   *  If `silent` is true, the typecheck is silent and will return `EmptyTree` if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Xlog-implicits.
   *  Unlike in `typeCheck`, `silent` is true by default.
   *
   *  @throws [[scala.reflect.macros.TypecheckException]]
   */
  def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree

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
}

/** Indicates an error during one of the methods in [[scala.reflect.macros.Typers]].
 */
case class TypecheckException(val pos: scala.reflect.api.Position, val msg: String) extends Exception(msg)
