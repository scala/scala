package scala.reflect.makro
package runtime

trait TypeTags {
  self: Context =>

  def TypeTag[T](tpe: Type): TypeTag[T] = mirror.TypeTag[T](mirror.rootMirror, mirror.FixedMirrorTypeCreator(mirror.rootMirror, tpe))
  def ConcreteTypeTag[T](tpe: Type): ConcreteTypeTag[T] = mirror.ConcreteTypeTag[T](mirror.rootMirror, mirror.FixedMirrorTypeCreator(mirror.rootMirror, tpe))
}
