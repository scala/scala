package scala

package object reflect {

  import ReflectionUtils._
  import scala.compat.Platform.EOL

  // !!! This was a val; we can't throw exceptions that aggressively without breaking
  // non-standard environments, e.g. google app engine.  I made it a lazy val, but
  // I think it would be better yet to throw the exception somewhere else - not during
  // initialization, but in response to a doomed attempt to utilize it.

  // todo. default mirror (a static object) might become a source for memory leaks (because it holds a strong reference to a classloader)!
  lazy val mirror: api.Mirror =
    try mkMirror(defaultReflectionClassLoader)
    catch {
      case ex: UnsupportedOperationException =>
        new DummyMirror(defaultReflectionClassLoader)
    }

  private[scala] def mirrorDiagnostics(cl: ClassLoader): String = """
    |
    | This error has happened because `scala.reflect.runtime.package` located in
    | scala-compiler.jar cannot be loaded. Classloader you are using is:
    | %s.
    |
    | For the instructions for some of the situations that might be relevant
    | visit our knowledge base at https://gist.github.com/2391081.
  """.stripMargin('|').format(show(cl))

  def mkMirror(classLoader: ClassLoader): api.Mirror = {
    val coreClassLoader = getClass.getClassLoader
    val instance = invokeFactoryOpt(coreClassLoader, "scala.reflect.runtime.package", "mkMirror", classLoader)
    instance match {
      case Some(x: api.Mirror) => x
      case Some(_) => throw new UnsupportedOperationException("Available scala reflection implementation is incompatible with this interface." + mirrorDiagnostics(coreClassLoader))
      case None => throw new UnsupportedOperationException("Scala reflection not available on this platform." + mirrorDiagnostics(coreClassLoader))
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

  // ArrayTag trait is defined separately from the mirror
  // ErasureTag trait is defined separately from the mirror
  // ConcreteErasureTag trait is defined separately from the mirror
  // ClassTag class is defined separately from the mirror
  type TypeTag[T]          = scala.reflect.mirror.TypeTag[T]
  type ConcreteTypeTag[T]  = scala.reflect.mirror.ConcreteTypeTag[T]

  // ClassTag object is defined separately from the mirror
  lazy val TypeTag         = scala.reflect.mirror.TypeTag
  lazy val ConcreteTypeTag = scala.reflect.mirror.ConcreteTypeTag

  def arrayTagToClassManifest[T](tag: ArrayTag[T]): ClassManifest[T] = TagInterop.arrayTagToClassManifest[T](tag)
  def concreteTypeTagToManifest[T](tag: ConcreteTypeTag[T]): Manifest[T] = TagInterop.concreteTypeTagToManifest[T](tag)
  def manifestToConcreteTypeTag[T](tag: Manifest[T]): ConcreteTypeTag[T] = TagInterop.manifestToConcreteTypeTag[T](tag)
}
