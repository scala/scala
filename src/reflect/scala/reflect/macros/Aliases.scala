package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.Context the Scala macros context]] that defines shorthands for the
 *  most frequently used types and functions of the underlying compiler universe.
 */
trait Aliases {
  self: Context =>

  /** The type of symbols representing declarations. */
  type Symbol = universe.Symbol

  /** The type of Scala types, and also Scala type signatures.
   *  (No difference is internally made between the two).
   */
  type Type = universe.Type

  /** The abstract type of names. */
  type Name = universe.Name

  /** The abstract type of names representing terms. */
  type TermName = universe.TermName

  /** The abstract type of names representing types. */
  type TypeName = universe.TypeName

  /** The type of Scala abstract syntax trees. */
  type Tree = universe.Tree

  /** Defines a universe-specific notion of positions. */
  type Position = universe.Position

  /** The base type of all scopes. */
  type Scope = universe.Scope

  /** The type of tree modifiers. */
  type Modifiers = universe.Modifiers

  /** The type of compilation runs. */
  type Run = universe.Run

  /** The type of compilation units. */
  type CompilationUnit = universe.CompilationUnit

  /** Expr wraps an abstract syntax tree and tags it with its type. */
  type Expr[+T] = universe.Expr[T]

  /** Constructor/Extractor for `Expr`. */
  val Expr = universe.Expr

  /** A shorthand to create an expr.
   *
   *  Unlike the conventional expr factory, which requires a [[scala.reflect.api.TreeCreator]],
   *  this one accepts a regular tree, but the resulting exprs are unable of being migrated
   *  to other universes/mirrors (the functionality normally not needed for macros, since there is
   *  only one compile-time universe and only one compile-time mirror).
   */
  def Expr[T: WeakTypeTag](tree: Tree): Expr[T]

  /** The type of weak type tags. */
  type WeakTypeTag[T] = universe.WeakTypeTag[T]

  /** The type of type tags. */
  type TypeTag[T] = universe.TypeTag[T]

  /** Constructor/Extractor for `WeakTypeTag`. */
  val WeakTypeTag = universe.WeakTypeTag

  /** Constructor/Extractor for `TypeTag`. */
  val TypeTag = universe.TypeTag

  /** A shorthand to create a weak type tag.
   *
   *  Unlike the conventional type tag factory, which requires a [[scala.reflect.api.TypeCreator]],
   *  this one accepts a regular type, but the resulting type tags are unable of being migrated
   *  to other universes/mirrors (the functionality normally not needed for macros, since there is
   *  only one compile-time universe and only one compile-time mirror).
   */
  def WeakTypeTag[T](tpe: Type): WeakTypeTag[T]

  /** A shorthand to create a type tag.
   *
   *  Unlike the conventional type tag factory, which requires a [[scala.reflect.api.TypeCreator]],
   *  this one accepts a regular type, but the resulting type tags are unable of being migrated
   *  to other universes/mirrors (the functionality normally not needed for macros, since there is
   *  only one compile-time universe and only one compile-time mirror).
   */
  def TypeTag[T](tpe: Type): TypeTag[T]

  /**
   * Shortcut for `implicitly[WeakTypeTag[T]]`
   */
  def weakTypeTag[T](implicit attag: WeakTypeTag[T]) = attag

  /**
   * Shortcut for `implicitly[TypeTag[T]]`
   */
  def typeTag[T](implicit ttag: TypeTag[T]) = ttag

  /**
   * Shortcut for `implicitly[WeakTypeTag[T]].tpe`
   */
  def weakTypeOf[T](implicit attag: WeakTypeTag[T]): Type = attag.tpe

  /**
   * Shortcut for `implicitly[TypeTag[T]].tpe`
   */
  def typeOf[T](implicit ttag: TypeTag[T]): Type = ttag.tpe
}
