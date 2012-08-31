package scala.reflect
package macros

trait Aliases {
  self: Context =>

  type Symbol = universe.Symbol
  type Type = universe.Type
  type Name = universe.Name
  type TermName = universe.TermName
  type TypeName = universe.TypeName
  type Tree = universe.Tree
  // type Position = universe.Position
  type Scope = universe.Scope
  type Modifiers = universe.Modifiers

  type Expr[+T] = universe.Expr[T]
  val Expr = universe.Expr

  type AbsTypeTag[T] = universe.AbsTypeTag[T]
  type TypeTag[T] = universe.TypeTag[T]
  val AbsTypeTag = universe.AbsTypeTag
  val TypeTag = universe.TypeTag
  def absTypeTag[T](implicit attag: AbsTypeTag[T]) = attag
  def typeTag[T](implicit ttag: TypeTag[T]) = ttag
  def absTypeOf[T](implicit attag: AbsTypeTag[T]): Type = attag.tpe
  def typeOf[T](implicit ttag: TypeTag[T]): Type = ttag.tpe
}
