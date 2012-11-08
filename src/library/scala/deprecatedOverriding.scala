/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** An annotation that designates that overriding a member is deprecated.
 *
 *  Overriding such a member in  a sub-class then generates a warning.
 *
 *  @param  message the message to print during compilation if the member was overridden
 *  @param  since   a string identifying the first version in which overriding was deprecated
 *  @since  2.10
 *  @see    [[scala.deprecatedInheritance]]
 */
private[scala] // for the same reasons as deprecatedInheritance
class deprecatedOverriding(message: String = "", since: String = "") extends scala.annotation.StaticAnnotation
