package scala
package reflect
package api

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  This trait provides type tag <-> manifest interoperability.
 *  @group ReflectionAPI
 *
 *  @groupname TagInterop TypeTag and Manifest Interoperability
 */
trait TagInterop { self: Universe =>
  // TODO `mirror` parameters are now of type `Any`, because I can't make these path-dependent types work
  // if you're brave enough, replace `Any` with `Mirror`, recompile and run interop_typetags_are_manifests.scala

  /**
   * Convert a [[scala.reflect.api.TypeTags#TypeTag]] to a [[scala.reflect.Manifest]].
   *
   * Compiler usually generates these conversions automatically, when a type tag for a type `T` is in scope,
   * and an implicit of type `Manifest[T]` is requested, but this method can also be called manually.
   * For example:
   * {{{
   * typeTagToManifest(scala.reflect.runtime.currentMirror, implicitly[TypeTag[String]])
   * }}}
   * @group TagInterop
   */
  def typeTagToManifest[T: ClassTag](mirror: Any, tag: Universe#TypeTag[T]): Manifest[T] =
    throw new UnsupportedOperationException("This universe does not support tag -> manifest conversions. Use a JavaUniverse, e.g. the scala.reflect.runtime.universe.")

  /**
   * Convert a [[scala.reflect.Manifest]] to a [[scala.reflect.api.TypeTags#TypeTag]].
   *
   * Compiler usually generates these conversions automatically, when a manifest for a type `T` is in scope,
   * and an implicit of type `TypeTag[T]` is requested, but this method can also be called manually.
   * For example:
   * {{{
   * manifestToTypeTag(scala.reflect.runtime.currentMirror, implicitly[Manifest[String]])
   * }}}
   * @group TagInterop
   */
  def manifestToTypeTag[T](mirror: Any, manifest: Manifest[T]): Universe#TypeTag[T] =
    throw new UnsupportedOperationException("This universe does not support manifest -> tag conversions. Use a JavaUniverse, e.g. the scala.reflect.runtime.universe.")
}
