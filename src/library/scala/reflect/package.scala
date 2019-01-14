/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import java.lang.reflect.{ AccessibleObject => jAccessibleObject }

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

  /** A `Manifest[T]` is an opaque descriptor for type T.  Its supported use
   *  is to give access to the erasure of the type as a `Class` instance, as
   *  is necessary for the creation of native `Arrays` if the class is not
   *  known at compile time.
   *
   *  The type-relation operators `<:<` and `=:=` should be considered
   *  approximations only, as there are numerous aspects of type conformance
   *  which are not yet adequately represented in manifests.
   *
   *  Example usages:
   *  {{{
   *    def arr[T] = new Array[T](0)                          // does not compile
   *    def arr[T](implicit m: Manifest[T]) = new Array[T](0) // compiles
   *    def arr[T: Manifest] = new Array[T](0)                // shorthand for the preceding
   *
   *    // Methods manifest and optManifest are in [[scala.Predef]].
   *    def isApproxSubType[T: Manifest, U: Manifest] = manifest[T] <:< manifest[U]
   *    isApproxSubType[List[String], List[AnyRef]] // true
   *    isApproxSubType[List[String], List[Int]]    // false
   *
   *    def methods[T: Manifest] = manifest[T].runtimeClass.getMethods
   *    def retType[T: Manifest](name: String) =
   *      methods[T] find (_.getName == name) map (_.getGenericReturnType)
   *
   *    retType[Map[_, _]]("values")  // Some(scala.collection.Iterable<B>)
   *  }}}
   */
  @scala.annotation.implicitNotFound(msg = "No Manifest available for ${T}.")
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("use scala.reflect.ClassTag (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
  trait Manifest[T] extends ClassManifest[T] with Equals {
    override def typeArguments: List[Manifest[_]] = Nil

    override def arrayManifest: Manifest[Array[T]] =
      Manifest.classType[Array[T]](arrayClass[T](runtimeClass), this)

    override def canEqual(that: Any): Boolean = that match {
      case _: Manifest[_]   => true
      case _                => false
    }
    /** Note: testing for erasure here is important, as it is many times
     *  faster than <:< and rules out most comparisons.
     */
    override def equals(that: Any): Boolean = that match {
      case m: Manifest[_] => (m canEqual this) && (this.runtimeClass == m.runtimeClass) && (this <:< m) && (m <:< this)
      case _              => false
    }
    override def hashCode = this.runtimeClass.##
  }

  /** The object `Manifest` defines factory methods for manifests.
   *  It is intended for use by the compiler and should not be used in client code.
   */
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("use scala.reflect.ClassTag (to capture erasures), scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
  val Manifest = ManifestFactory

  def classTag[T](implicit ctag: ClassTag[T]) = ctag

  /** Make a java reflection object accessible, if it is not already
   *  and it is possible to do so. If a SecurityException is thrown in the
   *  attempt, it is caught and discarded.
   */
  def ensureAccessible[T <: jAccessibleObject](m: T): T = {
    if (!m.isAccessible) {
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
