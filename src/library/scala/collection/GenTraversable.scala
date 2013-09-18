/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import generic._

/** A trait for all traversable collections which may possibly
 *  have their operations implemented in parallel.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenTraversable[+A]
extends GenTraversableLike[A, GenTraversable[A]]
   with GenTraversableOnce[A]
   with GenericTraversableTemplate[A, GenTraversable]
{
  def seq: Traversable[A]
  def companion: GenericCompanion[GenTraversable] = GenTraversable
}

object GenTraversable extends GenTraversableFactory[GenTraversable] {
  implicit def canBuildFrom[A] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A] = Traversable.newBuilder
}
