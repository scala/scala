package scala.reflect
package api

trait JavaMirrors { self: JavaUniverse =>

  type RuntimeClass = java.lang.Class[_]

  override type Mirror >: Null <: JavaMirror

  trait JavaMirror extends scala.reflect.api.Mirror[self.type] with RuntimeMirror {
    val classLoader: ClassLoader
    override def toString = s"JavaMirror with ${runtime.ReflectionUtils.show(classLoader)}"
  }

  def runtimeMirror(cl: ClassLoader): Mirror
}
