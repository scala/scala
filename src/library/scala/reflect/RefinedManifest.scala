/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

trait RefinedManifest[T] extends Manifest[T] {
  override def canEqual(other: Any) = other match {
    case _: RefinedManifest[_] => true
    case _                     => false
  }

  /** Tests whether the type represented by this manifest is equal to
    * the type represented by `that` manifest, subject to the limitations
    * described in the header.
    */
  override def equals(that: Any): Boolean = that match {
    case m: RefinedManifest[_] => (m canEqual this) && (this.erasure == m.erasure)
    case _                     => false
  }
  override def hashCode = this.erasure.##

  def fields: List[(String, Manifest[_])]
}
