/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection


import generic._


/** A trait for all iterable collections which may possibly
 *  have their operations implemented in parallel.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenIterable[+A]
extends GenIterableLike[A, GenIterable[A]]
   with GenTraversable[A]
   with GenericTraversableTemplate[A, GenIterable]
{
  def seq: Iterable[A]
  override def companion: GenericCompanion[GenIterable] = GenIterable
}


object GenIterable extends GenTraversableFactory[GenIterable] {
  implicit def canBuildFrom[A] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A] = Iterable.newBuilder
}

