package scala.reflect.makro
package runtime

trait Aliases {
  self: Context =>

  override type Symbol = universe.Symbol
  override type Type = universe.Type
  override type Name = universe.Name
  override type TermName = universe.TermName
  override type TypeName = universe.TypeName
  override type Tree = universe.Tree
  // override type Position = universe.Position
  override type Scope = universe.Scope
  override type Modifiers = universe.Modifiers

  override type Expr[+T] = universe.Expr[T]
  override val Expr = universe.Expr

  override type TypeTag[T] = universe.TypeTag[T]
  override type ConcreteTypeTag[T] = universe.ConcreteTypeTag[T]
  override val TypeTag = universe.TypeTag
  override val ConcreteTypeTag = universe.ConcreteTypeTag
  override def typeTag[T](implicit ttag: TypeTag[T]) = ttag
  override def concreteTypeTag[T](implicit cttag: ConcreteTypeTag[T]) = cttag
  override def typeOf[T](implicit ttag: TypeTag[T]): Type = ttag.tpe
}