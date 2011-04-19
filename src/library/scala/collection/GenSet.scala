/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection


import generic._


/** A trait for sets which may possibly
 *  have their operations implemented in parallel.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenSet[A]
extends GenSetLike[A, GenSet[A]]
   with GenIterable[A]
   with GenericSetTemplate[A, GenSet]
{
  override def companion: GenericCompanion[GenSet] = GenSet
  def seq: Set[A]
}


object GenSet extends TraversableFactory[GenSet] {
  implicit def canBuildFrom[A] = new GenericCanBuildFrom[A]
  def newBuilder[A] = Set.newBuilder
}

