package scala.reflect
package base

abstract class TreeCreator {
  def apply[U <: Universe with Singleton](m: MirrorOf[U]): U # Tree
}
