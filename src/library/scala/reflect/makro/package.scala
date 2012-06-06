package scala.reflect

package object makro {

  type MirrorOf[U <: base.Universe with Singleton] = base.MirrorOf[U]
}
