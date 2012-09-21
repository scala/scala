package scala.reflect

package object internal {

  type MirrorOf[U <: scala.reflect.api.Universe with Singleton] = scala.reflect.api.MirrorOf[U]
}
