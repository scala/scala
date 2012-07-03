package scala

package object reflect {

  lazy val basis: base.Universe = new base.Base

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
  @deprecated("Use scala.reflect.ClassTag instead", "2.10.0")
  @annotation.implicitNotFound(msg = "No ClassManifest available for ${T}.")
  type ClassManifest[T]  = scala.reflect.ClassTag[T]

  /** The object `ClassManifest` defines factory methods for manifests.
   *  It is intended for use by the compiler and should not be used in client code.
   */
  @deprecated("Use scala.reflect.ClassTag instead", "2.10.0")
  val ClassManifest = ClassManifestFactory

  /** The object `Manifest` defines factory methods for manifests.
   *  It is intended for use by the compiler and should not be used in client code.
   */
  @deprecated("Use scala.reflect.ClassTag (to capture erasures), scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
  val Manifest = ManifestFactory

  def classTag[T](implicit ctag: ClassTag[T]) = ctag
  // typeTag incantation is defined inside scala.reflect.basis and scala.reflect.runtime.universe

  // ClassTag class is defined in ClassTag.scala
  type TypeTag[T]        = scala.reflect.basis.TypeTag[T]

  // ClassTag object is defined in ClassTag.scala
  lazy val TypeTag       = scala.reflect.basis.TypeTag

  @deprecated("Use `@scala.beans.BeanDescription` instead", "2.10.0")
  type BeanDescription = scala.beans.BeanDescription
  @deprecated("Use `@scala.beans.BeanDisplayName` instead", "2.10.0")
  type BeanDisplayName = scala.beans.BeanDisplayName
  @deprecated("Use `@scala.beans.BeanInfo` instead", "2.10.0")
  type BeanInfo = scala.beans.BeanInfo
  @deprecated("Use `@scala.beans.BeanInfoSkip` instead", "2.10.0")
  type BeanInfoSkip = scala.beans.BeanInfoSkip
  @deprecated("Use `@scala.beans.BeanProperty` instead", "2.10.0")
  type BeanProperty = scala.beans.BeanProperty
  @deprecated("Use `@scala.beans.BooleanBeanProperty` instead", "2.10.0")
  type BooleanBeanProperty = scala.beans.BooleanBeanProperty
  @deprecated("Use `@scala.beans.ScalaBeanInfo` instead", "2.10.0")
  type ScalaBeanInfo = scala.beans.ScalaBeanInfo
}
