package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.blackbox.Context the Scala macros context]] that
 *  partially exposes the type checker to macro writers.
 */
trait Typers {
  self: blackbox.Context =>

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
  def openMacros: List[blackbox.Context]

  /** @see `Typers.typecheck`
   */
  @deprecated("Use `c.typecheck` instead", "2.11.0")
  def typeCheck(tree: Tree, pt: Type = universe.WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree =
    typecheck(tree, pt, silent, withImplicitViewsDisabled, withMacrosDisabled)

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
  def typecheck(tree: Tree, pt: Type = universe.WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree

  /** Infers an implicit value of the expected type `pt` in the macro callsite context.
   *  Optional `pos` parameter provides a position that will be associated with the implicit search.
   *
   *  If `silent` is false, `TypecheckException` will be thrown in case of an inference error.
   *  If `silent` is true, the typecheck is silent and will return `EmptyTree` if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Xlog-implicits.
   *  Unlike in `typecheck`, `silent` is true by default.
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
   *  Unlike in `typecheck`, `silent` is true by default.
   *
   *  @throws [[scala.reflect.macros.TypecheckException]]
   */
  def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition): Tree

  /** Recursively resets symbols and types in a given tree.
   *  WARNING: Don't use this API, go for [[untypecheck]] instead.
   */
  @deprecated("Use `c.untypecheck` instead", "2.11.0")
  def resetAllAttrs(tree: Tree): Tree

  /** Recursively resets locally defined symbols and types in a given tree.
   *  WARNING: Don't use this API, go for [[untypecheck]] instead.
   */
  @deprecated("Use `c.untypecheck` instead", "2.11.0")
  def resetLocalAttrs(tree: Tree): Tree

  /** In the current implementation of Scala's reflection API, untyped trees (also known as parser trees or unattributed trees)
   *  are observationally different from typed trees (also known as typer trees, typechecked trees or attributed trees),
   *
   *  Usually, if some compiler API takes a tree, then both untyped and typed trees will do. However in some cases,
   *  only untyped or only typed trees are appropriate. For example, [[eval]] only accepts untyped trees and one can only splice
   *  typed trees inside typed trees. Therefore in the current reflection API, there is a need in functions
   *  that go back and forth between untyped and typed trees. For this we have [[typecheck]] and `untypecheck`.
   *
   *  Note that `untypecheck` is currently afflicted by https://issues.scala-lang.org/browse/SI-5464,
   *  which makes it sometimes corrupt trees so that they don't make sense anymore. Unfortunately, there's no workaround for that.
   *  We plan to fix this issue soon, but for now please keep it in mind.
   *
   *  @see [[http://stackoverflow.com/questions/20936509/scala-macros-what-is-the-difference-between-typed-aka-typechecked-an-untyped]]
   */
  def untypecheck(tree: Tree): Tree
}

/** Indicates an error during one of the methods in [[scala.reflect.macros.Typers]].
 */
case class TypecheckException(pos: scala.reflect.api.Position, msg: String) extends Exception(msg)
