/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import java.lang.reflect.{AccessibleObject => jAccessibleObject}
import scala.annotation.nowarn

package object reflect {

  // in the new scheme of things ClassManifests are aliased to ClassTags
  // this is done because we want `toArray` in collections work with ClassTags
  // but changing it to use the ClassTag context bound without aliasing ClassManifest
  // will break everyone who subclasses and overrides `toArray`
  // luckily for us, aliasing doesn't hamper backward compatibility, so it's ideal in this situation
  // I wish we could do the same for Manifests and TypeTags though

  // note, by the way, that we don't touch ClassManifest the object
  // because its Byte, Short and so on factory fields are incompatible with ClassTag's

  /** A `ClassManifest[T]` is an opaque descriptor for type `T`.
   *  It is used by the compiler to preserve information necessary
   *  for instantiating `Arrays` in those cases where the element type
   *  is unknown at compile time.
   *
   *  The type-relation operators make an effort to present a more accurate
   *  picture than can be realized with erased types, but they should not be
   *  relied upon to give correct answers. In particular they are likely to
   *  be wrong when variance is involved or when a subtype has a different
   *  number of type arguments than a supertype.
   */
  @deprecated("use scala.reflect.ClassTag instead", "2.10.0")
  @annotation.implicitNotFound(msg = "No ClassManifest available for ${T}.")
  type ClassManifest[T]  = scala.reflect.ClassTag[T]

  /** The object `ClassManifest` defines factory methods for manifests.
   *  It is intended for use by the compiler and should not be used in client code.
   */
  @deprecated("use scala.reflect.ClassTag instead", "2.10.0")
  val ClassManifest = ClassManifestFactory

  def classTag[T](implicit ctag: ClassTag[T]) = ctag

  /** Make a java reflection object accessible, if it is not already
   *  and it is possible to do so. If a SecurityException is thrown in the
   *  attempt, it is caught and discarded.
   */
  def ensureAccessible[T <: jAccessibleObject](m: T): T = {
    // This calls `setAccessible` unnecessarily, because `isAccessible` is only `true` if `setAccessible(true)`
    // was called before, not if the reflected object is inherently accessible.
    // TODO: replace by `canAccess` once we're on JDK 9+
    if (!m.isAccessible: @nowarn("cat=deprecation")) {
      try m setAccessible true
      catch { case _: SecurityException => } // does nothing
    }
    m
  }

  // anchor for the class tag materialization macro emitted during tag materialization in Implicits.scala
  // implementation is hardwired into `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  // todo. once we have implicit macros for tag generation, we can remove this anchor
  private[scala] def materializeClassTag[T](): ClassTag[T] = macro ???
}

/** An exception that indicates an error during Scala reflection */
case class ScalaReflectionException(msg: String) extends Exception(msg)
