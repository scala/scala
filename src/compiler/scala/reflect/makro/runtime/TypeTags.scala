package scala.reflect.makro
package runtime

trait TypeTags {
  self: Context =>

  def TypeTag[T](tpe: Type): TypeTag[T] = universe.TypeTag[T](mirror, universe.FixedMirrorTypeCreator(mirror, tpe))
  def ConcreteTypeTag[T](tpe: Type): ConcreteTypeTag[T] = universe.ConcreteTypeTag[T](mirror, universe.FixedMirrorTypeCreator(mirror, tpe))
}
