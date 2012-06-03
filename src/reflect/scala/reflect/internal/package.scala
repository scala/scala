package scala.reflect

package object internal {

  type MirrorOf[U <: base.Universe with Singleton] = base.MirrorOf[U]
}
