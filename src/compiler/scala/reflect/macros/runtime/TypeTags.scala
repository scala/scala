package scala.reflect.macros
package runtime

trait TypeTags {
  self: Context =>

  def WeakTypeTag[T](tpe: Type): WeakTypeTag[T] = universe.WeakTypeTag[T](mirror, universe.FixedMirrorTypeCreator(mirror, tpe))
  def TypeTag[T](tpe: Type): TypeTag[T] = universe.TypeTag[T](mirror, universe.FixedMirrorTypeCreator(mirror, tpe))
}
