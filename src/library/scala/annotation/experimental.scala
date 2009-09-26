/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation

/** <p>
 *    An annotation for experimental features.
 *  </p>
 *
 *  @since 2.8
 */
final class experimental(message: String) extends StaticAnnotation {
  def this() = this("")
}
