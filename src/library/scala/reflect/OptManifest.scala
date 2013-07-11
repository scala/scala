/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package reflect

/** A `OptManifest[T]` is an optional [[scala.reflect.Manifest]].
 *
 *  It is either a `Manifest` or the value `NoManifest`.
 *
 *  @author Martin Odersky
 */
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("This notion doesn't have a corresponding concept in 2.10, because scala.reflect.runtime.universe.TypeTag can capture arbitrary types. Use type tags instead of manifests, and there will be no need in opt manifests.", "2.10.0")
trait OptManifest[+T] extends Serializable