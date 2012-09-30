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
  type Position = universe.Position
  type Scope = universe.Scope
  type Modifiers = universe.Modifiers
  type Run = universe.Run
  type CompilationUnit = universe.CompilationUnit

  type Expr[+T] = universe.Expr[T]
  val Expr = universe.Expr
  def Expr[T: WeakTypeTag](tree: Tree): Expr[T]

  type WeakTypeTag[T] = universe.WeakTypeTag[T]
  type TypeTag[T] = universe.TypeTag[T]
  val WeakTypeTag = universe.WeakTypeTag
  val TypeTag = universe.TypeTag
  def WeakTypeTag[T](tpe: Type): WeakTypeTag[T]
  def TypeTag[T](tpe: Type): TypeTag[T]
  def weakTypeTag[T](implicit attag: WeakTypeTag[T]) = attag
  def typeTag[T](implicit ttag: TypeTag[T]) = ttag
  def weakTypeOf[T](implicit attag: WeakTypeTag[T]): Type = attag.tpe
  def typeOf[T](implicit ttag: TypeTag[T]): Type = ttag.tpe
}
