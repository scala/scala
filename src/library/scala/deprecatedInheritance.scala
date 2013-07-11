/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** An annotation that designates that inheriting from a class is deprecated.
 *
 *  This is usually done to warn about a non-final class being made final in a future version.
 *  Sub-classing such a class then generates a warning. No warnings are generated if the
 *  subclass is in the same compilation unit.
 *
 *  @param  message the message to print during compilation if the class was sub-classed
 *  @param  since   a string identifying the first version in which inheritance was deprecated
 *  @since  2.10
 *  @see    [[scala.deprecatedOverriding]]
 */
private[scala] // for now, this needs to be generalized to communicate other modifier deltas
class deprecatedInheritance(message: String = "", since: String = "") extends scala.annotation.StaticAnnotation
