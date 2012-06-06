package scala.reflect
package base

abstract class TypeCreator {
  def apply[U <: Universe with Singleton](m: MirrorOf[U]): U # Type
}
