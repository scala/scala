package scala.reflect.makro
package runtime

trait Aliases {
  self: Context =>

  /** Aliases of mirror types */
  override type Symbol = mirror.Symbol
  override type Type = mirror.Type
  override type Name = mirror.Name
  override type Tree = mirror.Tree
  override type Position = mirror.Position
  override type Scope = mirror.Scope
  override type Modifiers = mirror.Modifiers
  override type Expr[+T] = mirror.Expr[T]
  override type TypeTag[T] = mirror.TypeTag[T]

  /** Creator/extractor objects for Expr and TypeTag values */
  override val TypeTag = mirror.TypeTag
  override val Expr = mirror.Expr
}