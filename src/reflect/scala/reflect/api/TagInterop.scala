package scala.reflect
package api

trait TagInterop { self: Universe =>
  // TODO `mirror` parameters are now of type `Any`, because I can't make these path-dependent types work
  // if you're brave enough, replace `Any` with `Mirror`, recompile and run interop_typetags_are_manifests.scala

  /**
   * Convert a typetag to a pre `Scala-2.10` manifest.
   * For example
   * {{{
   * typeTagToManifest( scala.reflect.runtime.currentMirror, implicitly[TypeTag[String]] )
   * }}}
   */
  def typeTagToManifest[T: ClassTag](mirror: Any, tag: Universe#TypeTag[T]): Manifest[T] =
    throw new UnsupportedOperationException("This universe does not support tag -> manifest conversions. Use a JavaUniverse, e.g. the scala.reflect.runtime.universe.")

  /**
   * Convert a pre `Scala-2.10` manifest to a typetag.
   * For example
   * {{{
   * manifestToTypeTag( scala.reflect.runtime.currentMirror, implicitly[Manifest[String]] )
   * }}}
   */
  def manifestToTypeTag[T](mirror: Any, manifest: Manifest[T]): Universe#TypeTag[T] =
    throw new UnsupportedOperationException("This universe does not support manifest -> tag conversions. Use a JavaUniverse, e.g. the scala.reflect.runtime.universe.")
}
