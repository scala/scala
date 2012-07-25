package scala.reflect.macros
package runtime

trait TypeTags {
  self: Context =>

  def AbsTypeTag[T](tpe: Type): AbsTypeTag[T] = universe.AbsTypeTag[T](mirror, universe.FixedMirrorTypeCreator(mirror, tpe))
  def TypeTag[T](tpe: Type): TypeTag[T] = universe.TypeTag[T](mirror, universe.FixedMirrorTypeCreator(mirror, tpe))
}
