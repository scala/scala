/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

/** One of the branches of an [[scala.reflect.OptManifest]].
  */
@deprecated("Use `@scala.reflect.TypeTag` instead", "2.10.0")
object NoManifest extends OptManifest[Nothing] with Serializable {
  override def toString = "<?>"
}