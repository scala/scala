package scala.reflect

package object runtime {
  def mkMirror(classLoader: ClassLoader): api.Mirror = new Mirror(classLoader)
}