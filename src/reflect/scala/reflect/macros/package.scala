package scala.reflect

package object macros {

  type MirrorOf[U <: base.Universe with Singleton] = base.MirrorOf[U]
}
