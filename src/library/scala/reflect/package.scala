package scala

package object reflect {

  import ReflectionUtils._

  // !!! This was a val; we can't throw exceptions that aggressively without breaking
  // non-standard environments, e.g. google app engine.  I made it a lazy val, but
  // I think it would be better yet to throw the exception somewhere else - not during
  // initialization, but in response to a doomed attempt to utilize it.

  // todo. default mirror (a static object) might become a source for memory leaks (because it holds a strong reference to a classloader)!
  lazy val mirror: api.Mirror = mkMirror(defaultReflectionClassLoader)

  def mkMirror(classLoader: ClassLoader): api.Mirror = {
    // we use (Java) reflection here so that we can keep reflect.runtime and reflect.internals in a seperate jar
    // note that we must instantiate the mirror with current classloader, otherwise we won't be able to cast it to api.Mirror
    // that's not a problem, though, because mirror can service classes from arbitrary classloaders
    val instance = invokeFactoryOpt(getClass.getClassLoader, "scala.reflect.runtime.package", "mkMirror", classLoader)
    instance match {
      case Some(x: api.Mirror) => x
      case Some(_) => throw new UnsupportedOperationException("Available scala reflection implementation is incompatible with this interface")
      case None => throw new UnsupportedOperationException("Scala reflection not available on this platform")
    }
  }

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

  @deprecated("Use `@scala.reflect.ClassTag` instead", "2.10.0")
  type ClassManifest[T] = ClassTag[T]
  @deprecated("OptManifest is no longer supported, and using it may lead to incorrect results, Use `@scala.reflect.TypeTag` instead", "2.10.0")
  type OptManifest[T]   = TypeTag[T]
  @deprecated("Use `@scala.reflect.ConcreteTypeTag` instead", "2.10.0")
  type Manifest[T]      = ConcreteTypeTag[T]

  @deprecated("Use `@scala.reflect.ClassTag` instead", "2.10.0")
  val ClassManifest     = ClassTag
  @deprecated("Use `@scala.reflect.ConcreteTypeTag` instead", "2.10.0")
  lazy val Manifest     = ConcreteTypeTag
  @deprecated("NoManifest is no longer supported, and using it may lead to incorrect results, Use `@scala.reflect.TypeTag` instead", "2.10.0")
  object NoManifest extends OptManifest[Nothing](scala.reflect.mirror.definitions.NothingClass.asType) with Serializable

  // ClassTag class is defined separately from the mirror
  type TypeTag[T]          = scala.reflect.mirror.TypeTag[T]
  type ConcreteTypeTag[T]  = scala.reflect.mirror.ConcreteTypeTag[T]

  // ClassTag object is defined separately from the mirror
  lazy val TypeTag         = scala.reflect.mirror.TypeTag
  lazy val ConcreteTypeTag = scala.reflect.mirror.ConcreteTypeTag
}
