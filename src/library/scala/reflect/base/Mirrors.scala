package scala.reflect
package base

/**
 * Defines a type hierarchy for mirrors.
 *
 * Every universe has one or more mirrors. A mirror defines a hierarchy of symbols starting with the root package `_root_` 
 * and provides methods to locate and define classes and singleton objects in that hierarchy.
 *
 * On the JVM, there is a one to one correspondance between class loaders and mirrors.
 */
trait Mirrors {
  self: Universe =>

  /** The base type of all mirrors of this universe */
  type Mirror >: Null <: MirrorOf[self.type]

  /** The root mirror of this universe. This mirror contains standard Scala classes and types such as `Any`, `AnyRef`, `AnyVal`, 
   *  `Nothing`, `Null`, and all classes loaded from scala-library, which are shared across all mirrors within the enclosing universe.
   */
  val rootMirror: Mirror
}
