/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.mutable

import generic._

/** A subtrait of collection.Traversable which represents traversables
 *  that can be mutated.
 *
 *  @author   Martin Odersky
 *  @version 2.8
 */
trait Traversable[A] extends collection.Traversable[A]
                        with TraversableClass[A, Traversable]
                        with TraversableTemplate[A, Traversable[A]]
                        with Mutable {
  override def companion: Companion[Traversable] = Traversable
}

/* A factory object for the trait `Traversable` */
object Traversable extends TraversableFactory[Traversable] {
  implicit def builderFactory[A]: BuilderFactory[A, Traversable[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Traversable[A]] = new ArrayBuffer
}


