package scala.reflect
package base

trait Mirrors {
  self: Universe =>

  /** .. */
  type Mirror >: Null <: MirrorOf[self.type]

  /** .. */
  val rootMirror: Mirror
}
