/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/**
 * An annotation that marks a member as having changed semantics
 * between versions.  This is intended for methods which for one
 * reason or another retain the same name and type signature,
 * but some aspect of their behavior is different.  An illustrative
 * examples is Stack.iterator, which reversed from LIFO to FIFO
 * order between scala 2.7 and 2.8.
 *
 * The version numbers are to mark the scala major/minor release
 * version where the change took place.
 *
 * @since 2.8
 */
private[scala] final class migration(
  majorVersion: Int,
  minorVersion: Int,
  message: String)
extends StaticAnnotation {}
