package scala.reflect

package object macros {

  type MirrorOf[U <: scala.reflect.api.Universe with Singleton] = scala.reflect.api.MirrorOf[U]
}
