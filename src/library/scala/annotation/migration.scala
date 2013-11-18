/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
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
 * order between Scala 2.7 and 2.8.
 *
 * @param message A message describing the change, which is emitted
 * by the compiler if the flag `-Xmigration` indicates a version
 * prior to the changedIn version.
 *
 * @param changedIn The version, in which the behaviour change was
 * introduced.
 *
 * @since 2.8
 */
 private[scala] final class migration(message: String, changedIn: String) extends scala.annotation.StaticAnnotation
