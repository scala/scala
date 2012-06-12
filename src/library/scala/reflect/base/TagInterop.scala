package scala.reflect
package base

import scala.runtime.ScalaRunTime._

trait TagInterop { self: Universe =>
  def classTagToClassManifest[T](tag: ClassTag[T]): ClassManifest[T] = {
    val runtimeClass = tag.runtimeClass
    if (runtimeClass.isArray) {
      val elementClass = arrayElementClass(runtimeClass)
      val elementManifest = classTagToClassManifest(ClassTag(elementClass))
      ClassManifest.arrayType(elementManifest).asInstanceOf[ClassManifest[T]]
    } else {
      ClassManifest.fromClass(runtimeClass.asInstanceOf[Class[T]])
    }
  }

  // [Eugene++] `mirror` parameters are now of type `Any`, because I can't make these path-dependent types work
  // if you're brave enough, replace `Any` with `Mirror`, recompile and run interop_typetags_are_manifests.scala

  // [Eugene++] would be great if we could approximate the interop without any mirrors
  // todo. think how to implement that

  def typeTagToManifest[T: ClassTag](mirror: Any, tag: base.Universe # TypeTag[T]): Manifest[T] =
    throw new UnsupportedOperationException("This universe does not support tag -> manifest conversions. Use scala.reflect.runtime.universe from scala-reflect.jar.")

  def manifestToTypeTag[T](mirror: Any, manifest: Manifest[T]): base.Universe # TypeTag[T] =
    throw new UnsupportedOperationException("This universe does not support manifest -> tag conversions. Use scala.reflect.runtime.universe from scala-reflect.jar.")
}
