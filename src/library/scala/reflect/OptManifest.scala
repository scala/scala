/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

/** A `OptManifest[T]` is an optional [[scala.reflect.Manifest]].
 *
 *  It is either a `Manifest` or the value `NoManifest`.
 *
 *  @author Martin Odersky
 */
@deprecated("Use `@scala.reflect.TypeTag` instead", "2.10.0")
trait OptManifest[+T] extends Serializable