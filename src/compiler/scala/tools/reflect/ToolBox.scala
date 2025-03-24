/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package reflect

trait ToolBox[U <: scala.reflect.api.Universe] {

  /** Underlying universe of a ToolBox
   */
  val u: U

  /** Underlying mirror of a ToolBox
   */
  val mirror: u.Mirror

  /** Front end of the toolbox.
   *
   *  Accumulates and displays warnings and errors, can drop to interactive mode (if supported).
   *  The latter can be useful to study the typechecker or to debug complex macros.
   *
   *  [[scala.tools.reflect]] provides two predefined front ends that can be created using
   *  [[scala.tools.reflect.mkSilentFrontEnd]] and [[scala.tools.reflect.mkConsoleFrontEnd]].
   */
  def frontEnd: FrontEnd

  /** Represents mode of operations of the typechecker underlying `c.typecheck` calls.
   *  Is necessary since the shape of the typechecked tree alone is not enough to guess how it should be typechecked.
   *  Can be TERMmode (typecheck as a term), TYPEmode (typecheck as a type) or PATTERNmode (typecheck as a pattern).
   */
  type TypecheckMode

  /** Indicates that an argument to `c.typecheck` should be typechecked as a term.
   *  This is the default typechecking mode in Scala 2.11 and the only one supported in Scala 2.10.
   */
  val TERMmode: TypecheckMode

  /** Indicates that an argument to `c.typecheck` should be typechecked as a type.
   */
  val TYPEmode: TypecheckMode

  /** Indicates that an argument to `c.typecheck` should be typechecked as a pattern.
   */
  val PATTERNmode: TypecheckMode

  /** @see `Typers.typecheck`
   */
  @deprecated("Use `tb.typecheck` instead", "2.11.0")
  def typeCheck(tree: u.Tree, pt: u.Type = u.WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): u.Tree =
    typecheck(tree, TERMmode, pt, silent, withImplicitViewsDisabled, withMacrosDisabled)

  /** Typechecks a tree against the expected type `pt`
   *  under typechecking mode specified in `mode` with [[TERMmode]] being default.
   *  This populates symbols and types of the tree and possibly transforms it to reflect certain desugarings.
   *
   *  If the tree has unresolved type variables (represented as instances of `FreeTypeSymbol` symbols),
   *  then they all have to be resolved first using `Tree.substituteTypes`, or an error occurs.
   *
   *  If `silent` is false, `ToolBoxError` will be thrown in case of a typecheck error.
   *  If `silent` is true, the typecheck is silent and will return `EmptyTree` if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Vdebug.
   *
   *  Typechecking can be steered with the following optional parameters:
   *    `withImplicitViewsDisabled` recursively prohibits implicit views (though, implicit vals will still be looked up and filled in), default value is false
   *    `withMacrosDisabled` recursively prohibits macro expansions and macro-based implicits, default value is false
   */
  def typecheck(tree: u.Tree, mode: TypecheckMode = TERMmode, pt: u.Type = u.WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): u.Tree

  /** Infers an implicit value of the expected type `pt` in top-level context.
   *  Optional `pos` parameter provides a position that will be associated with the implicit search.
   *
   *  As mentioned in https://groups.google.com/forum/#!topic/scala-internals/ta-vbUT6JE8
   *  this API won't take into account the lexical context of the callsite, because
   *  currently it's impossible to reify it.
   *
   *  If `silent` is false, `ToolBoxError` will be thrown in case of an inference error.
   *  If `silent` is true, the typecheck is silent and will return `EmptyTree` if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Vimplicits.
   *  Unlike in `typecheck`, `silent` is true by default.
   */
  def inferImplicitValue(pt: u.Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: u.Position = u.NoPosition): u.Tree

  /** Infers an implicit view from the provided tree `tree` from the type `from` to the type `to` in the toplevel context.
   *  Optional `pos` parameter provides a position that will be associated with the implicit search.
   *
   *  As mentioned in https://groups.google.com/forum/#!topic/scala-internals/ta-vbUT6JE8
   *  this API won't take into account the lexical context of the callsite, because
   *  currently it's impossible to reify it.
   *
   *  If `silent` is false, `ToolBoxError` will be thrown in case of an inference error.
   *  If `silent` is true, the typecheck is silent and will return `EmptyTree` if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Vimplicits.
   *  Unlike in `typecheck`, `silent` is true by default.
   */
  def inferImplicitView(tree: u.Tree, from: u.Type, to: u.Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: u.Position = u.NoPosition): u.Tree

  /** Recursively resets locally defined symbols and types in a given tree.
   *  WARNING: Don't use this API, go for [[untypecheck]] instead.
   */
  @deprecated("Use `tb.untypecheck` instead", "2.11.0")
  def resetLocalAttrs(tree: u.Tree): u.Tree

  /**
   *  @see [[scala.reflect.macros.Typers.untypecheck]]
   */
  def untypecheck(tree: u.Tree): u.Tree

  /** .. */
  def parse(code: String): u.Tree

  /** Compiles a tree using this ToolBox.
   *
   *  If the tree has unresolved type variables (represented as instances of `FreeTypeSymbol` symbols),
   *  then they all have to be resolved first using `Tree.substituteTypes`, or an error occurs.
   *
   *  This spawns the compiler at the Namer phase, and pipelines the tree through that compiler.
   *  Currently `compile` does not accept trees that already typechecked, because typechecking isn't idempotent.
   *  For more info, take a look at https://github.com/scala/bug/issues/5464.
   */
  def compile(tree: u.Tree): () => Any

  /** Defines a top-level class, trait or module in this ToolBox,
   *  putting it into a uniquely-named package and returning a symbol that references the defined entity.
   *  For a ClassDef, a ClassSymbol is returned, and for a ModuleDef, a ModuleSymbol is returned (not a module class, but a module itself).
   *
   *  This method can be used to generate definitions that will later be re-used by subsequent calls to
   *  `compile`, `define` or `eval`. To refer to the generated definition in a tree, use q"\$sym".
   */
  def define(tree: u.ImplDef): u.Symbol

  /** Compiles and runs a tree using this ToolBox.
   *  Is equivalent to `compile(tree)()`.
   */
  def eval(tree: u.Tree): Any
}

/** Represents an error during toolboxing
 */
case class ToolBoxError(message: String, cause: Throwable = null) extends Throwable(message, cause)
