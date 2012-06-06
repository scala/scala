package scala.reflect
package makro

trait TypeTags {
  self: Context =>

  def TypeTag[T](tpe: Type): TypeTag[T]
  def ConcreteTypeTag[T](tpe: Type): ConcreteTypeTag[T]
}
