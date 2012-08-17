package scala.reflect
package api

trait JavaUniverse extends Universe with Mirrors with TagInterop { self =>

  type RuntimeClass = java.lang.Class[_]

  override type Mirror >: Null <: JavaMirror

  trait JavaMirror extends MirrorOf[self.type] with RuntimeMirror {
    val classLoader: ClassLoader
    override def toString = s"JavaMirror with ${runtime.ReflectionUtils.show(classLoader)}"
  }

  def runtimeMirror(cl: ClassLoader): Mirror
}

