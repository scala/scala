package scala.reflect
package makro

trait TypeTags {
  self: Context =>

  def AbsTypeTag[T](tpe: Type): AbsTypeTag[T]
  def TypeTag[T](tpe: Type): TypeTag[T]
}
