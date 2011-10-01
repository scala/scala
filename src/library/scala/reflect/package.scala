package scala

package object reflect extends ReflectionUtils {
  // !!! This was a val; we can't throw exceptions that aggressively without breaking
  // non-standard environments, e.g. google app engine.  I made it a lazy val, but
  // I think it would be better yet to throw the exception somewhere else - not during
  // initialization, but in response to a doomed attempt to utilize it.
  lazy val mirror: api.Mirror = {
    // we use (Java) reflection here so that we can keep reflect.runtime and reflect.internals in a seperate jar
    singletonInstance("scala.reflect.runtime.Mirror") collect { case x: api.Mirror => x } getOrElse {
      throw new UnsupportedOperationException("Scala reflection not available on this platform")
    }
  }

  /** Uncomment once we got rid of the old Symbols, Types, Trees
  type Symbol = mirror.Symbol
  type Type = mirror.Type
  type Tree = mirror.Tree
  */
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
