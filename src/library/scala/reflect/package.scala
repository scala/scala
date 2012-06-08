package scala

package object reflect extends reflect_compat {

  lazy val basis: base.Universe = new base.Base

  def classTag[T](implicit ctag: ClassTag[T]) = ctag
  // typeTag incantation is defined inside scala.reflect.basis and scala.reflect.runtime.universe

  // ClassTag class is defined in ClassTag.scala
  type TypeTag[T]          = scala.reflect.basis.TypeTag[T]

  // ClassTag object is defined in ClassTag.scala
  lazy val TypeTag         = scala.reflect.basis.TypeTag

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
