package scala.reflect.makro

trait Aliases {
  self: Context =>

  /** Aliases of mirror types */
  type Symbol = mirror.Symbol
  type Type = mirror.Type
  type Name = mirror.Name
  type Tree = mirror.Tree
  type Position = mirror.Position
  type Scope = mirror.Scope
  type Modifiers = mirror.Modifiers
  type Expr[+T] = mirror.Expr[T]
  type TypeTag[T] = mirror.TypeTag[T]

  /** Creator/extractor objects for Expr and TypeTag values */
  val TypeTag = mirror.TypeTag
  val Expr = mirror.Expr

  /** incantations for summoning tags */
  def tag[T](implicit ttag: TypeTag[T]) = ttag
  def typeTag[T](implicit ttag: TypeTag[T]) = ttag
  def concreteTag[T](implicit cttag: ConcreteTypeTag[T]) = cttag
  def concreteTypeTag[T](implicit cttag: ConcreteTypeTag[T]) = cttag
}
