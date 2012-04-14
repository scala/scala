/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic
import language.higherKinds
/**
 * @since 2.8
 */
trait GenericSetTemplate[A, +CC[X] <: GenSet[X]] extends GenericTraversableTemplate[A, CC] {
  def empty: CC[A] = companion.empty[A]
}

