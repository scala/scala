/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** `Null` is - together with [[scala.Nothing]] - at the bottom of the Scala type hierarchy.
 *
 *  `Null` is a subtype of all reference types; its only instance is the `null` reference.
 *  Since `Null` is not a subtype of value types, `null` is not a member of any such type.  For instance,
 *  it is not possible to assign `null` to a variable of type [[scala.Int]].
 */
sealed trait Null
