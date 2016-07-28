/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2016, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/**
 * `ValueOf[T]` provides the unique value of the type `T` where `T` is a type which has a
 * single inhabitant. Eligible types are singleton types of the form `stablePath.type`,
 * Unit and, if the -Yliteral-types compiler option has been specified, singleton types
 * corresponding to value literals.
 *
 * Instances of `ValueOf[T]` are provided implicitly for all eligible types. Typically
 * an instance would be required where a runtime value corresponding to a type level
 * computation is needed. For example, we might define a type of list which encodes its
 * length in its type, in which case the lenght of the list can be computed as a
 * runtime value directly from the type without needing either runtime storage or a
 * traversal of the list value,
 *
 * {{{
 * // List of T of length L ...
 * case class SList[T, L <: Int](l: List[T]) {
 *   def length(implicit l: ValueOf[L]): Int = l.value
 * }
 *
 * val l3 = Sized[Int, 3](List(1, 2, 3))
 * l3.length // == 3
 * }}}
 */
@scala.annotation.implicitNotFound(msg = "No singleton value available for ${T}.")
final class ValueOf[T](val value: T) extends AnyVal
