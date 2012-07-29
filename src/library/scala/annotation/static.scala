/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/**
 * An annotation that marks a member in the companion object as static
 * and ensures that the compiler generates static fields/methods for it.
 * This is important for Java interoperability and performance reasons.
 *
 * @since 2.10
 */
final class static extends StaticAnnotation {
  // TODO document exact semantics above!
}
