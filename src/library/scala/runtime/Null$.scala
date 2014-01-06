/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

/**
 * Dummy class which exist only to satisfy the JVM. It corresponds to
 * `scala.Null`. If such type appears in method signatures, it is erased
 * to this one. A private constructor ensures that Java code can't create
 * subclasses. The only value of type Null$ should be null
 */
sealed abstract class Null$ private ()
